{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.FailureException
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.FailureException where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a failure a contributor insights operation.
--
--
--
-- /See:/ 'failureException' smart constructor.
data FailureException = FailureException'
  { _feExceptionName ::
      !(Maybe Text),
    _feExceptionDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FailureException' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'feExceptionName' - Exception name.
--
-- * 'feExceptionDescription' - Description of the failure.
failureException ::
  FailureException
failureException =
  FailureException'
    { _feExceptionName = Nothing,
      _feExceptionDescription = Nothing
    }

-- | Exception name.
feExceptionName :: Lens' FailureException (Maybe Text)
feExceptionName = lens _feExceptionName (\s a -> s {_feExceptionName = a})

-- | Description of the failure.
feExceptionDescription :: Lens' FailureException (Maybe Text)
feExceptionDescription = lens _feExceptionDescription (\s a -> s {_feExceptionDescription = a})

instance FromJSON FailureException where
  parseJSON =
    withObject
      "FailureException"
      ( \x ->
          FailureException'
            <$> (x .:? "ExceptionName") <*> (x .:? "ExceptionDescription")
      )

instance Hashable FailureException

instance NFData FailureException
