{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.InstanceDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.InstanceDetails where

import Network.AWS.CostExplorer.Types.EC2InstanceDetails
import Network.AWS.CostExplorer.Types.ESInstanceDetails
import Network.AWS.CostExplorer.Types.ElastiCacheInstanceDetails
import Network.AWS.CostExplorer.Types.RDSInstanceDetails
import Network.AWS.CostExplorer.Types.RedshiftInstanceDetails
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details about the instances that AWS recommends that you purchase.
--
--
--
-- /See:/ 'instanceDetails' smart constructor.
data InstanceDetails = InstanceDetails'
  { _idESInstanceDetails ::
      !(Maybe ESInstanceDetails),
    _idRDSInstanceDetails :: !(Maybe RDSInstanceDetails),
    _idElastiCacheInstanceDetails ::
      !(Maybe ElastiCacheInstanceDetails),
    _idEC2InstanceDetails :: !(Maybe EC2InstanceDetails),
    _idRedshiftInstanceDetails ::
      !(Maybe RedshiftInstanceDetails)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idESInstanceDetails' - The Amazon ES instances that AWS recommends that you purchase.
--
-- * 'idRDSInstanceDetails' - The Amazon RDS instances that AWS recommends that you purchase.
--
-- * 'idElastiCacheInstanceDetails' - The ElastiCache instances that AWS recommends that you purchase.
--
-- * 'idEC2InstanceDetails' - The Amazon EC2 instances that AWS recommends that you purchase.
--
-- * 'idRedshiftInstanceDetails' - The Amazon Redshift instances that AWS recommends that you purchase.
instanceDetails ::
  InstanceDetails
instanceDetails =
  InstanceDetails'
    { _idESInstanceDetails = Nothing,
      _idRDSInstanceDetails = Nothing,
      _idElastiCacheInstanceDetails = Nothing,
      _idEC2InstanceDetails = Nothing,
      _idRedshiftInstanceDetails = Nothing
    }

-- | The Amazon ES instances that AWS recommends that you purchase.
idESInstanceDetails :: Lens' InstanceDetails (Maybe ESInstanceDetails)
idESInstanceDetails = lens _idESInstanceDetails (\s a -> s {_idESInstanceDetails = a})

-- | The Amazon RDS instances that AWS recommends that you purchase.
idRDSInstanceDetails :: Lens' InstanceDetails (Maybe RDSInstanceDetails)
idRDSInstanceDetails = lens _idRDSInstanceDetails (\s a -> s {_idRDSInstanceDetails = a})

-- | The ElastiCache instances that AWS recommends that you purchase.
idElastiCacheInstanceDetails :: Lens' InstanceDetails (Maybe ElastiCacheInstanceDetails)
idElastiCacheInstanceDetails = lens _idElastiCacheInstanceDetails (\s a -> s {_idElastiCacheInstanceDetails = a})

-- | The Amazon EC2 instances that AWS recommends that you purchase.
idEC2InstanceDetails :: Lens' InstanceDetails (Maybe EC2InstanceDetails)
idEC2InstanceDetails = lens _idEC2InstanceDetails (\s a -> s {_idEC2InstanceDetails = a})

-- | The Amazon Redshift instances that AWS recommends that you purchase.
idRedshiftInstanceDetails :: Lens' InstanceDetails (Maybe RedshiftInstanceDetails)
idRedshiftInstanceDetails = lens _idRedshiftInstanceDetails (\s a -> s {_idRedshiftInstanceDetails = a})

instance FromJSON InstanceDetails where
  parseJSON =
    withObject
      "InstanceDetails"
      ( \x ->
          InstanceDetails'
            <$> (x .:? "ESInstanceDetails")
            <*> (x .:? "RDSInstanceDetails")
            <*> (x .:? "ElastiCacheInstanceDetails")
            <*> (x .:? "EC2InstanceDetails")
            <*> (x .:? "RedshiftInstanceDetails")
      )

instance Hashable InstanceDetails

instance NFData InstanceDetails
