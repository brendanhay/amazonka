{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.FailedRemediationExceptionBatch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.FailedRemediationExceptionBatch where

import Network.AWS.Config.Types.RemediationException
import Network.AWS.Lens
import Network.AWS.Prelude

-- | List of each of the failed remediation exceptions with specific reasons.
--
--
--
-- /See:/ 'failedRemediationExceptionBatch' smart constructor.
data FailedRemediationExceptionBatch = FailedRemediationExceptionBatch'
  { _frebFailureMessage ::
      !(Maybe Text),
    _frebFailedItems ::
      !( Maybe
           [RemediationException]
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FailedRemediationExceptionBatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'frebFailureMessage' - Returns a failure message. For example, the auto-remediation has failed.
--
-- * 'frebFailedItems' - Returns remediation exception resource key object of the failed items.
failedRemediationExceptionBatch ::
  FailedRemediationExceptionBatch
failedRemediationExceptionBatch =
  FailedRemediationExceptionBatch'
    { _frebFailureMessage = Nothing,
      _frebFailedItems = Nothing
    }

-- | Returns a failure message. For example, the auto-remediation has failed.
frebFailureMessage :: Lens' FailedRemediationExceptionBatch (Maybe Text)
frebFailureMessage = lens _frebFailureMessage (\s a -> s {_frebFailureMessage = a})

-- | Returns remediation exception resource key object of the failed items.
frebFailedItems :: Lens' FailedRemediationExceptionBatch [RemediationException]
frebFailedItems = lens _frebFailedItems (\s a -> s {_frebFailedItems = a}) . _Default . _Coerce

instance FromJSON FailedRemediationExceptionBatch where
  parseJSON =
    withObject
      "FailedRemediationExceptionBatch"
      ( \x ->
          FailedRemediationExceptionBatch'
            <$> (x .:? "FailureMessage") <*> (x .:? "FailedItems" .!= mempty)
      )

instance Hashable FailedRemediationExceptionBatch

instance NFData FailedRemediationExceptionBatch
