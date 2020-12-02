{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.RedshiftInstanceDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.RedshiftInstanceDetails where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details about the Amazon Redshift instances that AWS recommends that you purchase.
--
--
--
-- /See:/ 'redshiftInstanceDetails' smart constructor.
data RedshiftInstanceDetails = RedshiftInstanceDetails'
  { _rCurrentGeneration ::
      !(Maybe Bool),
    _rFamily :: !(Maybe Text),
    _rSizeFlexEligible :: !(Maybe Bool),
    _rRegion :: !(Maybe Text),
    _rNodeType :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RedshiftInstanceDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rCurrentGeneration' - Whether the recommendation is for a current-generation instance.
--
-- * 'rFamily' - The instance family of the recommended reservation.
--
-- * 'rSizeFlexEligible' - Whether the recommended reservation is size flexible.
--
-- * 'rRegion' - The AWS Region of the recommended reservation.
--
-- * 'rNodeType' - The type of node that AWS recommends.
redshiftInstanceDetails ::
  RedshiftInstanceDetails
redshiftInstanceDetails =
  RedshiftInstanceDetails'
    { _rCurrentGeneration = Nothing,
      _rFamily = Nothing,
      _rSizeFlexEligible = Nothing,
      _rRegion = Nothing,
      _rNodeType = Nothing
    }

-- | Whether the recommendation is for a current-generation instance.
rCurrentGeneration :: Lens' RedshiftInstanceDetails (Maybe Bool)
rCurrentGeneration = lens _rCurrentGeneration (\s a -> s {_rCurrentGeneration = a})

-- | The instance family of the recommended reservation.
rFamily :: Lens' RedshiftInstanceDetails (Maybe Text)
rFamily = lens _rFamily (\s a -> s {_rFamily = a})

-- | Whether the recommended reservation is size flexible.
rSizeFlexEligible :: Lens' RedshiftInstanceDetails (Maybe Bool)
rSizeFlexEligible = lens _rSizeFlexEligible (\s a -> s {_rSizeFlexEligible = a})

-- | The AWS Region of the recommended reservation.
rRegion :: Lens' RedshiftInstanceDetails (Maybe Text)
rRegion = lens _rRegion (\s a -> s {_rRegion = a})

-- | The type of node that AWS recommends.
rNodeType :: Lens' RedshiftInstanceDetails (Maybe Text)
rNodeType = lens _rNodeType (\s a -> s {_rNodeType = a})

instance FromJSON RedshiftInstanceDetails where
  parseJSON =
    withObject
      "RedshiftInstanceDetails"
      ( \x ->
          RedshiftInstanceDetails'
            <$> (x .:? "CurrentGeneration")
            <*> (x .:? "Family")
            <*> (x .:? "SizeFlexEligible")
            <*> (x .:? "Region")
            <*> (x .:? "NodeType")
      )

instance Hashable RedshiftInstanceDetails

instance NFData RedshiftInstanceDetails
