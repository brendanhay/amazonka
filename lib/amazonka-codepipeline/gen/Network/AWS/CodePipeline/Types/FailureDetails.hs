{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.FailureDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.FailureDetails where

import Network.AWS.CodePipeline.Types.FailureType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents information about failure details.
--
--
--
-- /See:/ 'failureDetails' smart constructor.
data FailureDetails = FailureDetails'
  { _fdExternalExecutionId ::
      !(Maybe Text),
    _fdType :: !FailureType,
    _fdMessage :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FailureDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fdExternalExecutionId' - The external ID of the run of the action that failed.
--
-- * 'fdType' - The type of the failure.
--
-- * 'fdMessage' - The message about the failure.
failureDetails ::
  -- | 'fdType'
  FailureType ->
  -- | 'fdMessage'
  Text ->
  FailureDetails
failureDetails pType_ pMessage_ =
  FailureDetails'
    { _fdExternalExecutionId = Nothing,
      _fdType = pType_,
      _fdMessage = pMessage_
    }

-- | The external ID of the run of the action that failed.
fdExternalExecutionId :: Lens' FailureDetails (Maybe Text)
fdExternalExecutionId = lens _fdExternalExecutionId (\s a -> s {_fdExternalExecutionId = a})

-- | The type of the failure.
fdType :: Lens' FailureDetails FailureType
fdType = lens _fdType (\s a -> s {_fdType = a})

-- | The message about the failure.
fdMessage :: Lens' FailureDetails Text
fdMessage = lens _fdMessage (\s a -> s {_fdMessage = a})

instance Hashable FailureDetails

instance NFData FailureDetails

instance ToJSON FailureDetails where
  toJSON FailureDetails' {..} =
    object
      ( catMaybes
          [ ("externalExecutionId" .=) <$> _fdExternalExecutionId,
            Just ("type" .= _fdType),
            Just ("message" .= _fdMessage)
          ]
      )
