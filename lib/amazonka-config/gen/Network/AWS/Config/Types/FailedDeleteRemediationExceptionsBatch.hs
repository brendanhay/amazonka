{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.FailedDeleteRemediationExceptionsBatch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.FailedDeleteRemediationExceptionsBatch where

import Network.AWS.Config.Types.RemediationExceptionResourceKey
import Network.AWS.Lens
import Network.AWS.Prelude

-- | List of each of the failed delete remediation exceptions with specific reasons.
--
--
--
-- /See:/ 'failedDeleteRemediationExceptionsBatch' smart constructor.
data FailedDeleteRemediationExceptionsBatch = FailedDeleteRemediationExceptionsBatch'
  { _fdrebFailureMessage ::
      !(Maybe Text),
    _fdrebFailedItems ::
      !( Maybe
           ( List1
               RemediationExceptionResourceKey
           )
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FailedDeleteRemediationExceptionsBatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fdrebFailureMessage' - Returns a failure message for delete remediation exception. For example, AWS Config creates an exception due to an internal error.
--
-- * 'fdrebFailedItems' - Returns remediation exception resource key object of the failed items.
failedDeleteRemediationExceptionsBatch ::
  FailedDeleteRemediationExceptionsBatch
failedDeleteRemediationExceptionsBatch =
  FailedDeleteRemediationExceptionsBatch'
    { _fdrebFailureMessage =
        Nothing,
      _fdrebFailedItems = Nothing
    }

-- | Returns a failure message for delete remediation exception. For example, AWS Config creates an exception due to an internal error.
fdrebFailureMessage :: Lens' FailedDeleteRemediationExceptionsBatch (Maybe Text)
fdrebFailureMessage = lens _fdrebFailureMessage (\s a -> s {_fdrebFailureMessage = a})

-- | Returns remediation exception resource key object of the failed items.
fdrebFailedItems :: Lens' FailedDeleteRemediationExceptionsBatch (Maybe (NonEmpty RemediationExceptionResourceKey))
fdrebFailedItems = lens _fdrebFailedItems (\s a -> s {_fdrebFailedItems = a}) . mapping _List1

instance FromJSON FailedDeleteRemediationExceptionsBatch where
  parseJSON =
    withObject
      "FailedDeleteRemediationExceptionsBatch"
      ( \x ->
          FailedDeleteRemediationExceptionsBatch'
            <$> (x .:? "FailureMessage") <*> (x .:? "FailedItems")
      )

instance Hashable FailedDeleteRemediationExceptionsBatch

instance NFData FailedDeleteRemediationExceptionsBatch
