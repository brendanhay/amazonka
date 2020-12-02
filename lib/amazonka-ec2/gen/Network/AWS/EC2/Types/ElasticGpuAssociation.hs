{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ElasticGpuAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ElasticGpuAssociation where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the association between an instance and an Elastic Graphics accelerator.
--
--
--
-- /See:/ 'elasticGpuAssociation' smart constructor.
data ElasticGpuAssociation = ElasticGpuAssociation'
  { _egaElasticGpuId ::
      !(Maybe Text),
    _egaElasticGpuAssociationId :: !(Maybe Text),
    _egaElasticGpuAssociationTime :: !(Maybe Text),
    _egaElasticGpuAssociationState :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ElasticGpuAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'egaElasticGpuId' - The ID of the Elastic Graphics accelerator.
--
-- * 'egaElasticGpuAssociationId' - The ID of the association.
--
-- * 'egaElasticGpuAssociationTime' - The time the Elastic Graphics accelerator was associated with the instance.
--
-- * 'egaElasticGpuAssociationState' - The state of the association between the instance and the Elastic Graphics accelerator.
elasticGpuAssociation ::
  ElasticGpuAssociation
elasticGpuAssociation =
  ElasticGpuAssociation'
    { _egaElasticGpuId = Nothing,
      _egaElasticGpuAssociationId = Nothing,
      _egaElasticGpuAssociationTime = Nothing,
      _egaElasticGpuAssociationState = Nothing
    }

-- | The ID of the Elastic Graphics accelerator.
egaElasticGpuId :: Lens' ElasticGpuAssociation (Maybe Text)
egaElasticGpuId = lens _egaElasticGpuId (\s a -> s {_egaElasticGpuId = a})

-- | The ID of the association.
egaElasticGpuAssociationId :: Lens' ElasticGpuAssociation (Maybe Text)
egaElasticGpuAssociationId = lens _egaElasticGpuAssociationId (\s a -> s {_egaElasticGpuAssociationId = a})

-- | The time the Elastic Graphics accelerator was associated with the instance.
egaElasticGpuAssociationTime :: Lens' ElasticGpuAssociation (Maybe Text)
egaElasticGpuAssociationTime = lens _egaElasticGpuAssociationTime (\s a -> s {_egaElasticGpuAssociationTime = a})

-- | The state of the association between the instance and the Elastic Graphics accelerator.
egaElasticGpuAssociationState :: Lens' ElasticGpuAssociation (Maybe Text)
egaElasticGpuAssociationState = lens _egaElasticGpuAssociationState (\s a -> s {_egaElasticGpuAssociationState = a})

instance FromXML ElasticGpuAssociation where
  parseXML x =
    ElasticGpuAssociation'
      <$> (x .@? "elasticGpuId")
      <*> (x .@? "elasticGpuAssociationId")
      <*> (x .@? "elasticGpuAssociationTime")
      <*> (x .@? "elasticGpuAssociationState")

instance Hashable ElasticGpuAssociation

instance NFData ElasticGpuAssociation
