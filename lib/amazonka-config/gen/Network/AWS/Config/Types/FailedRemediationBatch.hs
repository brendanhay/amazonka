{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.FailedRemediationBatch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.FailedRemediationBatch where

import Network.AWS.Config.Types.RemediationConfiguration
import Network.AWS.Lens
import Network.AWS.Prelude

-- | List of each of the failed remediations with specific reasons.
--
--
--
-- /See:/ 'failedRemediationBatch' smart constructor.
data FailedRemediationBatch = FailedRemediationBatch'
  { _frbFailureMessage ::
      !(Maybe Text),
    _frbFailedItems ::
      !(Maybe [RemediationConfiguration])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FailedRemediationBatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'frbFailureMessage' - Returns a failure message. For example, the resource is already compliant.
--
-- * 'frbFailedItems' - Returns remediation configurations of the failed items.
failedRemediationBatch ::
  FailedRemediationBatch
failedRemediationBatch =
  FailedRemediationBatch'
    { _frbFailureMessage = Nothing,
      _frbFailedItems = Nothing
    }

-- | Returns a failure message. For example, the resource is already compliant.
frbFailureMessage :: Lens' FailedRemediationBatch (Maybe Text)
frbFailureMessage = lens _frbFailureMessage (\s a -> s {_frbFailureMessage = a})

-- | Returns remediation configurations of the failed items.
frbFailedItems :: Lens' FailedRemediationBatch [RemediationConfiguration]
frbFailedItems = lens _frbFailedItems (\s a -> s {_frbFailedItems = a}) . _Default . _Coerce

instance FromJSON FailedRemediationBatch where
  parseJSON =
    withObject
      "FailedRemediationBatch"
      ( \x ->
          FailedRemediationBatch'
            <$> (x .:? "FailureMessage") <*> (x .:? "FailedItems" .!= mempty)
      )

instance Hashable FailedRemediationBatch

instance NFData FailedRemediationBatch
