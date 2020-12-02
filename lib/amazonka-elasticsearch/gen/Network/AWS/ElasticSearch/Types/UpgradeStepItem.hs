{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.UpgradeStepItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.UpgradeStepItem where

import Network.AWS.ElasticSearch.Types.UpgradeStatus
import Network.AWS.ElasticSearch.Types.UpgradeStep
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a single step of the Upgrade or Upgrade Eligibility Check workflow.
--
--
--
-- /See:/ 'upgradeStepItem' smart constructor.
data UpgradeStepItem = UpgradeStepItem'
  { _usiUpgradeStepStatus ::
      !(Maybe UpgradeStatus),
    _usiProgressPercent :: !(Maybe Double),
    _usiIssues :: !(Maybe [Text]),
    _usiUpgradeStep :: !(Maybe UpgradeStep)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpgradeStepItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usiUpgradeStepStatus' - The status of a particular step during an upgrade. The status can take one of the following values:     * In Progress    * Succeeded    * Succeeded with Issues    * Failed
--
-- * 'usiProgressPercent' - The Floating point value representing progress percentage of a particular step.
--
-- * 'usiIssues' - A list of strings containing detailed information about the errors encountered in a particular step.
--
-- * 'usiUpgradeStep' - Represents one of 3 steps that an Upgrade or Upgrade Eligibility Check does through:     * PreUpgradeCheck    * Snapshot    * Upgrade
upgradeStepItem ::
  UpgradeStepItem
upgradeStepItem =
  UpgradeStepItem'
    { _usiUpgradeStepStatus = Nothing,
      _usiProgressPercent = Nothing,
      _usiIssues = Nothing,
      _usiUpgradeStep = Nothing
    }

-- | The status of a particular step during an upgrade. The status can take one of the following values:     * In Progress    * Succeeded    * Succeeded with Issues    * Failed
usiUpgradeStepStatus :: Lens' UpgradeStepItem (Maybe UpgradeStatus)
usiUpgradeStepStatus = lens _usiUpgradeStepStatus (\s a -> s {_usiUpgradeStepStatus = a})

-- | The Floating point value representing progress percentage of a particular step.
usiProgressPercent :: Lens' UpgradeStepItem (Maybe Double)
usiProgressPercent = lens _usiProgressPercent (\s a -> s {_usiProgressPercent = a})

-- | A list of strings containing detailed information about the errors encountered in a particular step.
usiIssues :: Lens' UpgradeStepItem [Text]
usiIssues = lens _usiIssues (\s a -> s {_usiIssues = a}) . _Default . _Coerce

-- | Represents one of 3 steps that an Upgrade or Upgrade Eligibility Check does through:     * PreUpgradeCheck    * Snapshot    * Upgrade
usiUpgradeStep :: Lens' UpgradeStepItem (Maybe UpgradeStep)
usiUpgradeStep = lens _usiUpgradeStep (\s a -> s {_usiUpgradeStep = a})

instance FromJSON UpgradeStepItem where
  parseJSON =
    withObject
      "UpgradeStepItem"
      ( \x ->
          UpgradeStepItem'
            <$> (x .:? "UpgradeStepStatus")
            <*> (x .:? "ProgressPercent")
            <*> (x .:? "Issues" .!= mempty)
            <*> (x .:? "UpgradeStep")
      )

instance Hashable UpgradeStepItem

instance NFData UpgradeStepItem
