{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.RDSInstanceDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.RDSInstanceDetails where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details about the Amazon RDS instances that AWS recommends that you purchase.
--
--
--
-- /See:/ 'rdsInstanceDetails' smart constructor.
data RDSInstanceDetails = RDSInstanceDetails'
  { _ridCurrentGeneration ::
      !(Maybe Bool),
    _ridDeploymentOption :: !(Maybe Text),
    _ridFamily :: !(Maybe Text),
    _ridInstanceType :: !(Maybe Text),
    _ridLicenseModel :: !(Maybe Text),
    _ridSizeFlexEligible :: !(Maybe Bool),
    _ridRegion :: !(Maybe Text),
    _ridDatabaseEngine :: !(Maybe Text),
    _ridDatabaseEdition :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RDSInstanceDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ridCurrentGeneration' - Whether the recommendation is for a current-generation instance.
--
-- * 'ridDeploymentOption' - Whether the recommendation is for a reservation in a single Availability Zone or a reservation with a backup in a second Availability Zone.
--
-- * 'ridFamily' - The instance family of the recommended reservation.
--
-- * 'ridInstanceType' - The type of instance that AWS recommends.
--
-- * 'ridLicenseModel' - The license model that the recommended reservation supports.
--
-- * 'ridSizeFlexEligible' - Whether the recommended reservation is size flexible.
--
-- * 'ridRegion' - The AWS Region of the recommended reservation.
--
-- * 'ridDatabaseEngine' - The database engine that the recommended reservation supports.
--
-- * 'ridDatabaseEdition' - The database edition that the recommended reservation supports.
rdsInstanceDetails ::
  RDSInstanceDetails
rdsInstanceDetails =
  RDSInstanceDetails'
    { _ridCurrentGeneration = Nothing,
      _ridDeploymentOption = Nothing,
      _ridFamily = Nothing,
      _ridInstanceType = Nothing,
      _ridLicenseModel = Nothing,
      _ridSizeFlexEligible = Nothing,
      _ridRegion = Nothing,
      _ridDatabaseEngine = Nothing,
      _ridDatabaseEdition = Nothing
    }

-- | Whether the recommendation is for a current-generation instance.
ridCurrentGeneration :: Lens' RDSInstanceDetails (Maybe Bool)
ridCurrentGeneration = lens _ridCurrentGeneration (\s a -> s {_ridCurrentGeneration = a})

-- | Whether the recommendation is for a reservation in a single Availability Zone or a reservation with a backup in a second Availability Zone.
ridDeploymentOption :: Lens' RDSInstanceDetails (Maybe Text)
ridDeploymentOption = lens _ridDeploymentOption (\s a -> s {_ridDeploymentOption = a})

-- | The instance family of the recommended reservation.
ridFamily :: Lens' RDSInstanceDetails (Maybe Text)
ridFamily = lens _ridFamily (\s a -> s {_ridFamily = a})

-- | The type of instance that AWS recommends.
ridInstanceType :: Lens' RDSInstanceDetails (Maybe Text)
ridInstanceType = lens _ridInstanceType (\s a -> s {_ridInstanceType = a})

-- | The license model that the recommended reservation supports.
ridLicenseModel :: Lens' RDSInstanceDetails (Maybe Text)
ridLicenseModel = lens _ridLicenseModel (\s a -> s {_ridLicenseModel = a})

-- | Whether the recommended reservation is size flexible.
ridSizeFlexEligible :: Lens' RDSInstanceDetails (Maybe Bool)
ridSizeFlexEligible = lens _ridSizeFlexEligible (\s a -> s {_ridSizeFlexEligible = a})

-- | The AWS Region of the recommended reservation.
ridRegion :: Lens' RDSInstanceDetails (Maybe Text)
ridRegion = lens _ridRegion (\s a -> s {_ridRegion = a})

-- | The database engine that the recommended reservation supports.
ridDatabaseEngine :: Lens' RDSInstanceDetails (Maybe Text)
ridDatabaseEngine = lens _ridDatabaseEngine (\s a -> s {_ridDatabaseEngine = a})

-- | The database edition that the recommended reservation supports.
ridDatabaseEdition :: Lens' RDSInstanceDetails (Maybe Text)
ridDatabaseEdition = lens _ridDatabaseEdition (\s a -> s {_ridDatabaseEdition = a})

instance FromJSON RDSInstanceDetails where
  parseJSON =
    withObject
      "RDSInstanceDetails"
      ( \x ->
          RDSInstanceDetails'
            <$> (x .:? "CurrentGeneration")
            <*> (x .:? "DeploymentOption")
            <*> (x .:? "Family")
            <*> (x .:? "InstanceType")
            <*> (x .:? "LicenseModel")
            <*> (x .:? "SizeFlexEligible")
            <*> (x .:? "Region")
            <*> (x .:? "DatabaseEngine")
            <*> (x .:? "DatabaseEdition")
      )

instance Hashable RDSInstanceDetails

instance NFData RDSInstanceDetails
