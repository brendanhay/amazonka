{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLContainerDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AutoMLContainerDefinition where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A list of container definitions that describe the different containers that make up one AutoML candidate. Refer to ContainerDefinition for more details.
--
--
--
-- /See:/ 'autoMLContainerDefinition' smart constructor.
data AutoMLContainerDefinition = AutoMLContainerDefinition'
  { _amlcdEnvironment ::
      !(Maybe (Map Text (Text))),
    _amlcdImage :: !Text,
    _amlcdModelDataURL :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AutoMLContainerDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'amlcdEnvironment' - Environment variables to set in the container. Refer to ContainerDefinition for more details.
--
-- * 'amlcdImage' - The ECR path of the container. Refer to ContainerDefinition for more details.
--
-- * 'amlcdModelDataURL' - The location of the model artifacts. Refer to ContainerDefinition for more details.
autoMLContainerDefinition ::
  -- | 'amlcdImage'
  Text ->
  -- | 'amlcdModelDataURL'
  Text ->
  AutoMLContainerDefinition
autoMLContainerDefinition pImage_ pModelDataURL_ =
  AutoMLContainerDefinition'
    { _amlcdEnvironment = Nothing,
      _amlcdImage = pImage_,
      _amlcdModelDataURL = pModelDataURL_
    }

-- | Environment variables to set in the container. Refer to ContainerDefinition for more details.
amlcdEnvironment :: Lens' AutoMLContainerDefinition (HashMap Text (Text))
amlcdEnvironment = lens _amlcdEnvironment (\s a -> s {_amlcdEnvironment = a}) . _Default . _Map

-- | The ECR path of the container. Refer to ContainerDefinition for more details.
amlcdImage :: Lens' AutoMLContainerDefinition Text
amlcdImage = lens _amlcdImage (\s a -> s {_amlcdImage = a})

-- | The location of the model artifacts. Refer to ContainerDefinition for more details.
amlcdModelDataURL :: Lens' AutoMLContainerDefinition Text
amlcdModelDataURL = lens _amlcdModelDataURL (\s a -> s {_amlcdModelDataURL = a})

instance FromJSON AutoMLContainerDefinition where
  parseJSON =
    withObject
      "AutoMLContainerDefinition"
      ( \x ->
          AutoMLContainerDefinition'
            <$> (x .:? "Environment" .!= mempty)
            <*> (x .: "Image")
            <*> (x .: "ModelDataUrl")
      )

instance Hashable AutoMLContainerDefinition

instance NFData AutoMLContainerDefinition
