{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.UpgradeTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.UpgradeTarget where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The version of the database engine that a DB instance can be upgraded to.
--
--
--
-- /See:/ 'upgradeTarget' smart constructor.
data UpgradeTarget = UpgradeTarget'
  { _utEngineVersion ::
      !(Maybe Text),
    _utIsMajorVersionUpgrade :: !(Maybe Bool),
    _utEngine :: !(Maybe Text),
    _utAutoUpgrade :: !(Maybe Bool),
    _utDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpgradeTarget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'utEngineVersion' - The version number of the upgrade target database engine.
--
-- * 'utIsMajorVersionUpgrade' - A value that indicates whether a database engine is upgraded to a major version.
--
-- * 'utEngine' - The name of the upgrade target database engine.
--
-- * 'utAutoUpgrade' - A value that indicates whether the target version is applied to any source DB instances that have @AutoMinorVersionUpgrade@ set to true.
--
-- * 'utDescription' - The version of the database engine that a DB instance can be upgraded to.
upgradeTarget ::
  UpgradeTarget
upgradeTarget =
  UpgradeTarget'
    { _utEngineVersion = Nothing,
      _utIsMajorVersionUpgrade = Nothing,
      _utEngine = Nothing,
      _utAutoUpgrade = Nothing,
      _utDescription = Nothing
    }

-- | The version number of the upgrade target database engine.
utEngineVersion :: Lens' UpgradeTarget (Maybe Text)
utEngineVersion = lens _utEngineVersion (\s a -> s {_utEngineVersion = a})

-- | A value that indicates whether a database engine is upgraded to a major version.
utIsMajorVersionUpgrade :: Lens' UpgradeTarget (Maybe Bool)
utIsMajorVersionUpgrade = lens _utIsMajorVersionUpgrade (\s a -> s {_utIsMajorVersionUpgrade = a})

-- | The name of the upgrade target database engine.
utEngine :: Lens' UpgradeTarget (Maybe Text)
utEngine = lens _utEngine (\s a -> s {_utEngine = a})

-- | A value that indicates whether the target version is applied to any source DB instances that have @AutoMinorVersionUpgrade@ set to true.
utAutoUpgrade :: Lens' UpgradeTarget (Maybe Bool)
utAutoUpgrade = lens _utAutoUpgrade (\s a -> s {_utAutoUpgrade = a})

-- | The version of the database engine that a DB instance can be upgraded to.
utDescription :: Lens' UpgradeTarget (Maybe Text)
utDescription = lens _utDescription (\s a -> s {_utDescription = a})

instance FromXML UpgradeTarget where
  parseXML x =
    UpgradeTarget'
      <$> (x .@? "EngineVersion")
      <*> (x .@? "IsMajorVersionUpgrade")
      <*> (x .@? "Engine")
      <*> (x .@? "AutoUpgrade")
      <*> (x .@? "Description")

instance Hashable UpgradeTarget

instance NFData UpgradeTarget
