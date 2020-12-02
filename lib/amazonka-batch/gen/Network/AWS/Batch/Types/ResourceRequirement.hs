{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.ResourceRequirement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.ResourceRequirement where

import Network.AWS.Batch.Types.ResourceType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The type and amount of a resource to assign to a container. Currently, the only supported resource type is @GPU@ .
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
-- * 'rrValue' - The number of physical GPUs to reserve for the container. The number of GPUs reserved for all containers in a job should not exceed the number of available GPUs on the compute resource that the job is launched on.
--
-- * 'rrType' - The type of resource to assign to a container. Currently, the only supported resource type is @GPU@ .
resourceRequirement ::
  -- | 'rrValue'
  Text ->
  -- | 'rrType'
  ResourceType ->
  ResourceRequirement
resourceRequirement pValue_ pType_ =
  ResourceRequirement' {_rrValue = pValue_, _rrType = pType_}

-- | The number of physical GPUs to reserve for the container. The number of GPUs reserved for all containers in a job should not exceed the number of available GPUs on the compute resource that the job is launched on.
rrValue :: Lens' ResourceRequirement Text
rrValue = lens _rrValue (\s a -> s {_rrValue = a})

-- | The type of resource to assign to a container. Currently, the only supported resource type is @GPU@ .
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
