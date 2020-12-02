{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.UpgradeHistory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.UpgradeHistory where

import Network.AWS.ElasticSearch.Types.UpgradeStatus
import Network.AWS.ElasticSearch.Types.UpgradeStepItem
import Network.AWS.Lens
import Network.AWS.Prelude

-- | History of the last 10 Upgrades and Upgrade Eligibility Checks.
--
--
--
-- /See:/ 'upgradeHistory' smart constructor.
data UpgradeHistory = UpgradeHistory'
  { _uhUpgradeStatus ::
      !(Maybe UpgradeStatus),
    _uhStepsList :: !(Maybe [UpgradeStepItem]),
    _uhUpgradeName :: !(Maybe Text),
    _uhStartTimestamp :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpgradeHistory' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uhUpgradeStatus' - The overall status of the update. The status can take one of the following values:     * In Progress    * Succeeded    * Succeeded with Issues    * Failed
--
-- * 'uhStepsList' - A list of @'UpgradeStepItem' @ s representing information about each step performed as pard of a specific Upgrade or Upgrade Eligibility Check.
--
-- * 'uhUpgradeName' - A string that describes the update briefly
--
-- * 'uhStartTimestamp' - UTC Timestamp at which the Upgrade API call was made in "yyyy-MM-ddTHH:mm:ssZ" format.
upgradeHistory ::
  UpgradeHistory
upgradeHistory =
  UpgradeHistory'
    { _uhUpgradeStatus = Nothing,
      _uhStepsList = Nothing,
      _uhUpgradeName = Nothing,
      _uhStartTimestamp = Nothing
    }

-- | The overall status of the update. The status can take one of the following values:     * In Progress    * Succeeded    * Succeeded with Issues    * Failed
uhUpgradeStatus :: Lens' UpgradeHistory (Maybe UpgradeStatus)
uhUpgradeStatus = lens _uhUpgradeStatus (\s a -> s {_uhUpgradeStatus = a})

-- | A list of @'UpgradeStepItem' @ s representing information about each step performed as pard of a specific Upgrade or Upgrade Eligibility Check.
uhStepsList :: Lens' UpgradeHistory [UpgradeStepItem]
uhStepsList = lens _uhStepsList (\s a -> s {_uhStepsList = a}) . _Default . _Coerce

-- | A string that describes the update briefly
uhUpgradeName :: Lens' UpgradeHistory (Maybe Text)
uhUpgradeName = lens _uhUpgradeName (\s a -> s {_uhUpgradeName = a})

-- | UTC Timestamp at which the Upgrade API call was made in "yyyy-MM-ddTHH:mm:ssZ" format.
uhStartTimestamp :: Lens' UpgradeHistory (Maybe UTCTime)
uhStartTimestamp = lens _uhStartTimestamp (\s a -> s {_uhStartTimestamp = a}) . mapping _Time

instance FromJSON UpgradeHistory where
  parseJSON =
    withObject
      "UpgradeHistory"
      ( \x ->
          UpgradeHistory'
            <$> (x .:? "UpgradeStatus")
            <*> (x .:? "StepsList" .!= mempty)
            <*> (x .:? "UpgradeName")
            <*> (x .:? "StartTimestamp")
      )

instance Hashable UpgradeHistory

instance NFData UpgradeHistory
