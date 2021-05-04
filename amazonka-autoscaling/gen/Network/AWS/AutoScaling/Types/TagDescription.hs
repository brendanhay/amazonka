{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.TagDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.TagDescription where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a tag for an Auto Scaling group.
--
-- /See:/ 'newTagDescription' smart constructor.
data TagDescription = TagDescription'
  { -- | The name of the group.
    resourceId :: Prelude.Text,
    -- | The type of resource. The only supported value is @auto-scaling-group@.
    resourceType :: Prelude.Text,
    -- | The tag key.
    key :: Prelude.Text,
    -- | Determines whether the tag is added to new instances as they are
    -- launched in the group.
    propagateAtLaunch :: Prelude.Bool,
    -- | The tag value.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TagDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'tagDescription_resourceId' - The name of the group.
--
-- 'resourceType', 'tagDescription_resourceType' - The type of resource. The only supported value is @auto-scaling-group@.
--
-- 'key', 'tagDescription_key' - The tag key.
--
-- 'propagateAtLaunch', 'tagDescription_propagateAtLaunch' - Determines whether the tag is added to new instances as they are
-- launched in the group.
--
-- 'value', 'tagDescription_value' - The tag value.
newTagDescription ::
  -- | 'resourceId'
  Prelude.Text ->
  -- | 'resourceType'
  Prelude.Text ->
  -- | 'key'
  Prelude.Text ->
  -- | 'propagateAtLaunch'
  Prelude.Bool ->
  -- | 'value'
  Prelude.Text ->
  TagDescription
newTagDescription
  pResourceId_
  pResourceType_
  pKey_
  pPropagateAtLaunch_
  pValue_ =
    TagDescription'
      { resourceId = pResourceId_,
        resourceType = pResourceType_,
        key = pKey_,
        propagateAtLaunch = pPropagateAtLaunch_,
        value = pValue_
      }

-- | The name of the group.
tagDescription_resourceId :: Lens.Lens' TagDescription Prelude.Text
tagDescription_resourceId = Lens.lens (\TagDescription' {resourceId} -> resourceId) (\s@TagDescription' {} a -> s {resourceId = a} :: TagDescription)

-- | The type of resource. The only supported value is @auto-scaling-group@.
tagDescription_resourceType :: Lens.Lens' TagDescription Prelude.Text
tagDescription_resourceType = Lens.lens (\TagDescription' {resourceType} -> resourceType) (\s@TagDescription' {} a -> s {resourceType = a} :: TagDescription)

-- | The tag key.
tagDescription_key :: Lens.Lens' TagDescription Prelude.Text
tagDescription_key = Lens.lens (\TagDescription' {key} -> key) (\s@TagDescription' {} a -> s {key = a} :: TagDescription)

-- | Determines whether the tag is added to new instances as they are
-- launched in the group.
tagDescription_propagateAtLaunch :: Lens.Lens' TagDescription Prelude.Bool
tagDescription_propagateAtLaunch = Lens.lens (\TagDescription' {propagateAtLaunch} -> propagateAtLaunch) (\s@TagDescription' {} a -> s {propagateAtLaunch = a} :: TagDescription)

-- | The tag value.
tagDescription_value :: Lens.Lens' TagDescription Prelude.Text
tagDescription_value = Lens.lens (\TagDescription' {value} -> value) (\s@TagDescription' {} a -> s {value = a} :: TagDescription)

instance Prelude.FromXML TagDescription where
  parseXML x =
    TagDescription'
      Prelude.<$> (x Prelude..@ "ResourceId")
      Prelude.<*> (x Prelude..@ "ResourceType")
      Prelude.<*> (x Prelude..@ "Key")
      Prelude.<*> (x Prelude..@ "PropagateAtLaunch")
      Prelude.<*> (x Prelude..@ "Value")

instance Prelude.Hashable TagDescription

instance Prelude.NFData TagDescription
