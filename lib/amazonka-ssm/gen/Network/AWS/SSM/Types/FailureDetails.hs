{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.FailureDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.FailureDetails where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about an Automation failure.
--
--
--
-- /See:/ 'failureDetails' smart constructor.
data FailureDetails = FailureDetails'
  { _fdFailureType ::
      !(Maybe Text),
    _fdFailureStage :: !(Maybe Text),
    _fdDetails :: !(Maybe (Map Text ([Text])))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FailureDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fdFailureType' - The type of Automation failure. Failure types include the following: Action, Permission, Throttling, Verification, Internal.
--
-- * 'fdFailureStage' - The stage of the Automation execution when the failure occurred. The stages include the following: InputValidation, PreVerification, Invocation, PostVerification.
--
-- * 'fdDetails' - Detailed information about the Automation step failure.
failureDetails ::
  FailureDetails
failureDetails =
  FailureDetails'
    { _fdFailureType = Nothing,
      _fdFailureStage = Nothing,
      _fdDetails = Nothing
    }

-- | The type of Automation failure. Failure types include the following: Action, Permission, Throttling, Verification, Internal.
fdFailureType :: Lens' FailureDetails (Maybe Text)
fdFailureType = lens _fdFailureType (\s a -> s {_fdFailureType = a})

-- | The stage of the Automation execution when the failure occurred. The stages include the following: InputValidation, PreVerification, Invocation, PostVerification.
fdFailureStage :: Lens' FailureDetails (Maybe Text)
fdFailureStage = lens _fdFailureStage (\s a -> s {_fdFailureStage = a})

-- | Detailed information about the Automation step failure.
fdDetails :: Lens' FailureDetails (HashMap Text ([Text]))
fdDetails = lens _fdDetails (\s a -> s {_fdDetails = a}) . _Default . _Map

instance FromJSON FailureDetails where
  parseJSON =
    withObject
      "FailureDetails"
      ( \x ->
          FailureDetails'
            <$> (x .:? "FailureType")
            <*> (x .:? "FailureStage")
            <*> (x .:? "Details" .!= mempty)
      )

instance Hashable FailureDetails

instance NFData FailureDetails
