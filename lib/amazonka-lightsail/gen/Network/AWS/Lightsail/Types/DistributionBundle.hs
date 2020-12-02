{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.DistributionBundle
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.DistributionBundle where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the specifications of a distribution bundle.
--
--
--
-- /See:/ 'distributionBundle' smart constructor.
data DistributionBundle = DistributionBundle'
  { _dbTransferPerMonthInGb ::
      !(Maybe Int),
    _dbBundleId :: !(Maybe Text),
    _dbName :: !(Maybe Text),
    _dbPrice :: !(Maybe Double),
    _dbIsActive :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DistributionBundle' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbTransferPerMonthInGb' - The monthly network transfer quota of the bundle.
--
-- * 'dbBundleId' - The ID of the bundle.
--
-- * 'dbName' - The name of the distribution bundle.
--
-- * 'dbPrice' - The monthly price, in US dollars, of the bundle.
--
-- * 'dbIsActive' - Indicates whether the bundle is active, and can be specified for a new distribution.
distributionBundle ::
  DistributionBundle
distributionBundle =
  DistributionBundle'
    { _dbTransferPerMonthInGb = Nothing,
      _dbBundleId = Nothing,
      _dbName = Nothing,
      _dbPrice = Nothing,
      _dbIsActive = Nothing
    }

-- | The monthly network transfer quota of the bundle.
dbTransferPerMonthInGb :: Lens' DistributionBundle (Maybe Int)
dbTransferPerMonthInGb = lens _dbTransferPerMonthInGb (\s a -> s {_dbTransferPerMonthInGb = a})

-- | The ID of the bundle.
dbBundleId :: Lens' DistributionBundle (Maybe Text)
dbBundleId = lens _dbBundleId (\s a -> s {_dbBundleId = a})

-- | The name of the distribution bundle.
dbName :: Lens' DistributionBundle (Maybe Text)
dbName = lens _dbName (\s a -> s {_dbName = a})

-- | The monthly price, in US dollars, of the bundle.
dbPrice :: Lens' DistributionBundle (Maybe Double)
dbPrice = lens _dbPrice (\s a -> s {_dbPrice = a})

-- | Indicates whether the bundle is active, and can be specified for a new distribution.
dbIsActive :: Lens' DistributionBundle (Maybe Bool)
dbIsActive = lens _dbIsActive (\s a -> s {_dbIsActive = a})

instance FromJSON DistributionBundle where
  parseJSON =
    withObject
      "DistributionBundle"
      ( \x ->
          DistributionBundle'
            <$> (x .:? "transferPerMonthInGb")
            <*> (x .:? "bundleId")
            <*> (x .:? "name")
            <*> (x .:? "price")
            <*> (x .:? "isActive")
      )

instance Hashable DistributionBundle

instance NFData DistributionBundle
