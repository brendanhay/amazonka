{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ElasticInferenceAcceleratorAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ElasticInferenceAcceleratorAssociation where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the association between an instance and an elastic inference accelerator.
--
--
--
-- /See:/ 'elasticInferenceAcceleratorAssociation' smart constructor.
data ElasticInferenceAcceleratorAssociation = ElasticInferenceAcceleratorAssociation'
  { _eiaaElasticInferenceAcceleratorAssociationState ::
      !(Maybe Text),
    _eiaaElasticInferenceAcceleratorAssociationTime ::
      !( Maybe
           ISO8601
       ),
    _eiaaElasticInferenceAcceleratorARN ::
      !(Maybe Text),
    _eiaaElasticInferenceAcceleratorAssociationId ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ElasticInferenceAcceleratorAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eiaaElasticInferenceAcceleratorAssociationState' - The state of the elastic inference accelerator.
--
-- * 'eiaaElasticInferenceAcceleratorAssociationTime' - The time at which the elastic inference accelerator is associated with an instance.
--
-- * 'eiaaElasticInferenceAcceleratorARN' - The Amazon Resource Name (ARN) of the elastic inference accelerator.
--
-- * 'eiaaElasticInferenceAcceleratorAssociationId' - The ID of the association.
elasticInferenceAcceleratorAssociation ::
  ElasticInferenceAcceleratorAssociation
elasticInferenceAcceleratorAssociation =
  ElasticInferenceAcceleratorAssociation'
    { _eiaaElasticInferenceAcceleratorAssociationState =
        Nothing,
      _eiaaElasticInferenceAcceleratorAssociationTime =
        Nothing,
      _eiaaElasticInferenceAcceleratorARN = Nothing,
      _eiaaElasticInferenceAcceleratorAssociationId = Nothing
    }

-- | The state of the elastic inference accelerator.
eiaaElasticInferenceAcceleratorAssociationState :: Lens' ElasticInferenceAcceleratorAssociation (Maybe Text)
eiaaElasticInferenceAcceleratorAssociationState = lens _eiaaElasticInferenceAcceleratorAssociationState (\s a -> s {_eiaaElasticInferenceAcceleratorAssociationState = a})

-- | The time at which the elastic inference accelerator is associated with an instance.
eiaaElasticInferenceAcceleratorAssociationTime :: Lens' ElasticInferenceAcceleratorAssociation (Maybe UTCTime)
eiaaElasticInferenceAcceleratorAssociationTime = lens _eiaaElasticInferenceAcceleratorAssociationTime (\s a -> s {_eiaaElasticInferenceAcceleratorAssociationTime = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the elastic inference accelerator.
eiaaElasticInferenceAcceleratorARN :: Lens' ElasticInferenceAcceleratorAssociation (Maybe Text)
eiaaElasticInferenceAcceleratorARN = lens _eiaaElasticInferenceAcceleratorARN (\s a -> s {_eiaaElasticInferenceAcceleratorARN = a})

-- | The ID of the association.
eiaaElasticInferenceAcceleratorAssociationId :: Lens' ElasticInferenceAcceleratorAssociation (Maybe Text)
eiaaElasticInferenceAcceleratorAssociationId = lens _eiaaElasticInferenceAcceleratorAssociationId (\s a -> s {_eiaaElasticInferenceAcceleratorAssociationId = a})

instance FromXML ElasticInferenceAcceleratorAssociation where
  parseXML x =
    ElasticInferenceAcceleratorAssociation'
      <$> (x .@? "elasticInferenceAcceleratorAssociationState")
      <*> (x .@? "elasticInferenceAcceleratorAssociationTime")
      <*> (x .@? "elasticInferenceAcceleratorArn")
      <*> (x .@? "elasticInferenceAcceleratorAssociationId")

instance Hashable ElasticInferenceAcceleratorAssociation

instance NFData ElasticInferenceAcceleratorAssociation
