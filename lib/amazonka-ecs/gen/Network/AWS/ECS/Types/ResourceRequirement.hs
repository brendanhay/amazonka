{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.ResourceRequirement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.ResourceRequirement where

import Network.AWS.ECS.Types.ResourceType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The type and amount of a resource to assign to a container. The supported resource types are GPUs and Elastic Inference accelerators. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-gpu.html Working with GPUs on Amazon ECS> or <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-eia.html Working with Amazon Elastic Inference on Amazon ECS> in the /Amazon Elastic Container Service Developer Guide/
--
--
--
-- /See:/ 'resourceRequirement' smart constructor.
data ResourceRequirement = ResourceRequirement'
  { _rrValue :: !Text,
    _rrType :: !ResourceType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResourceRequirement' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrValue' - The value for the specified resource type. If the @GPU@ type is used, the value is the number of physical @GPUs@ the Amazon ECS container agent will reserve for the container. The number of GPUs reserved for all containers in a task should not exceed the number of available GPUs on the container instance the task is launched on. If the @InferenceAccelerator@ type is used, the @value@ should match the @deviceName@ for an 'InferenceAccelerator' specified in a task definition.
--
-- * 'rrType' - The type of resource to assign to a container. The supported values are @GPU@ or @InferenceAccelerator@ .
resourceRequirement ::
  -- | 'rrValue'
  Text ->
  -- | 'rrType'
  ResourceType ->
  ResourceRequirement
resourceRequirement pValue_ pType_ =
  ResourceRequirement' {_rrValue = pValue_, _rrType = pType_}

-- | The value for the specified resource type. If the @GPU@ type is used, the value is the number of physical @GPUs@ the Amazon ECS container agent will reserve for the container. The number of GPUs reserved for all containers in a task should not exceed the number of available GPUs on the container instance the task is launched on. If the @InferenceAccelerator@ type is used, the @value@ should match the @deviceName@ for an 'InferenceAccelerator' specified in a task definition.
rrValue :: Lens' ResourceRequirement Text
rrValue = lens _rrValue (\s a -> s {_rrValue = a})

-- | The type of resource to assign to a container. The supported values are @GPU@ or @InferenceAccelerator@ .
rrType :: Lens' ResourceRequirement ResourceType
rrType = lens _rrType (\s a -> s {_rrType = a})

instance FromJSON ResourceRequirement where
  parseJSON =
    withObject
      "ResourceRequirement"
      (\x -> ResourceRequirement' <$> (x .: "value") <*> (x .: "type"))

instance Hashable ResourceRequirement

instance NFData ResourceRequirement

instance ToJSON ResourceRequirement where
  toJSON ResourceRequirement' {..} =
    object
      (catMaybes [Just ("value" .= _rrValue), Just ("type" .= _rrType)])
