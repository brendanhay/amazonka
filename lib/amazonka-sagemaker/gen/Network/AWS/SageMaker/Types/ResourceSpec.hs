{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ResourceSpec
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ResourceSpec where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.AppInstanceType

-- | Specifies the ARN's of a SageMaker image and SageMaker image version, and the instance type that the version runs on.
--
--
--
-- /See:/ 'resourceSpec' smart constructor.
data ResourceSpec = ResourceSpec'
  { _rsInstanceType ::
      !(Maybe AppInstanceType),
    _rsSageMakerImageARN :: !(Maybe Text),
    _rsSageMakerImageVersionARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResourceSpec' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsInstanceType' - The instance type that the image version runs on.
--
-- * 'rsSageMakerImageARN' - The ARN of the SageMaker image that the image version belongs to.
--
-- * 'rsSageMakerImageVersionARN' - The ARN of the image version created on the instance.
resourceSpec ::
  ResourceSpec
resourceSpec =
  ResourceSpec'
    { _rsInstanceType = Nothing,
      _rsSageMakerImageARN = Nothing,
      _rsSageMakerImageVersionARN = Nothing
    }

-- | The instance type that the image version runs on.
rsInstanceType :: Lens' ResourceSpec (Maybe AppInstanceType)
rsInstanceType = lens _rsInstanceType (\s a -> s {_rsInstanceType = a})

-- | The ARN of the SageMaker image that the image version belongs to.
rsSageMakerImageARN :: Lens' ResourceSpec (Maybe Text)
rsSageMakerImageARN = lens _rsSageMakerImageARN (\s a -> s {_rsSageMakerImageARN = a})

-- | The ARN of the image version created on the instance.
rsSageMakerImageVersionARN :: Lens' ResourceSpec (Maybe Text)
rsSageMakerImageVersionARN = lens _rsSageMakerImageVersionARN (\s a -> s {_rsSageMakerImageVersionARN = a})

instance FromJSON ResourceSpec where
  parseJSON =
    withObject
      "ResourceSpec"
      ( \x ->
          ResourceSpec'
            <$> (x .:? "InstanceType")
            <*> (x .:? "SageMakerImageArn")
            <*> (x .:? "SageMakerImageVersionArn")
      )

instance Hashable ResourceSpec

instance NFData ResourceSpec

instance ToJSON ResourceSpec where
  toJSON ResourceSpec' {..} =
    object
      ( catMaybes
          [ ("InstanceType" .=) <$> _rsInstanceType,
            ("SageMakerImageArn" .=) <$> _rsSageMakerImageARN,
            ("SageMakerImageVersionArn" .=) <$> _rsSageMakerImageVersionARN
          ]
      )
