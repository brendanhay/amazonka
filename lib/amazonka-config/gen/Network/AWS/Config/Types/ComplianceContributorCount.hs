{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ComplianceContributorCount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ComplianceContributorCount where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The number of AWS resources or AWS Config rules responsible for the current compliance of the item, up to a maximum number.
--
--
--
-- /See:/ 'complianceContributorCount' smart constructor.
data ComplianceContributorCount = ComplianceContributorCount'
  { _cccCappedCount ::
      !(Maybe Int),
    _cccCapExceeded :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ComplianceContributorCount' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cccCappedCount' - The number of AWS resources or AWS Config rules responsible for the current compliance of the item.
--
-- * 'cccCapExceeded' - Indicates whether the maximum count is reached.
complianceContributorCount ::
  ComplianceContributorCount
complianceContributorCount =
  ComplianceContributorCount'
    { _cccCappedCount = Nothing,
      _cccCapExceeded = Nothing
    }

-- | The number of AWS resources or AWS Config rules responsible for the current compliance of the item.
cccCappedCount :: Lens' ComplianceContributorCount (Maybe Int)
cccCappedCount = lens _cccCappedCount (\s a -> s {_cccCappedCount = a})

-- | Indicates whether the maximum count is reached.
cccCapExceeded :: Lens' ComplianceContributorCount (Maybe Bool)
cccCapExceeded = lens _cccCapExceeded (\s a -> s {_cccCapExceeded = a})

instance FromJSON ComplianceContributorCount where
  parseJSON =
    withObject
      "ComplianceContributorCount"
      ( \x ->
          ComplianceContributorCount'
            <$> (x .:? "CappedCount") <*> (x .:? "CapExceeded")
      )

instance Hashable ComplianceContributorCount

instance NFData ComplianceContributorCount
