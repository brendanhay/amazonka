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
-- Module      : Network.AWS.AutoScaling.Types.Tag
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.Tag where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a tag for an Auto Scaling group.
--
-- /See:/ 'newTag' smart constructor.
data Tag = Tag'
  { -- | The tag key.
    key :: Prelude.Text,
    -- | The name of the group.
    resourceId :: Prelude.Text,
    -- | The type of resource. The only supported value is @auto-scaling-group@.
    resourceType :: Prelude.Text,
    -- | Determines whether the tag is added to new instances as they are
    -- launched in the group.
    propagateAtLaunch :: Prelude.Bool,
    -- | The tag value.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Tag' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'tag_key' - The tag key.
--
-- 'resourceId', 'tag_resourceId' - The name of the group.
--
-- 'resourceType', 'tag_resourceType' - The type of resource. The only supported value is @auto-scaling-group@.
--
-- 'propagateAtLaunch', 'tag_propagateAtLaunch' - Determines whether the tag is added to new instances as they are
-- launched in the group.
--
-- 'value', 'tag_value' - The tag value.
newTag ::
  -- | 'key'
  Prelude.Text ->
  -- | 'resourceId'
  Prelude.Text ->
  -- | 'resourceType'
  Prelude.Text ->
  -- | 'propagateAtLaunch'
  Prelude.Bool ->
  -- | 'value'
  Prelude.Text ->
  Tag
newTag
  pKey_
  pResourceId_
  pResourceType_
  pPropagateAtLaunch_
  pValue_ =
    Tag'
      { key = pKey_,
        resourceId = pResourceId_,
        resourceType = pResourceType_,
        propagateAtLaunch = pPropagateAtLaunch_,
        value = pValue_
      }

-- | The tag key.
tag_key :: Lens.Lens' Tag Prelude.Text
tag_key = Lens.lens (\Tag' {key} -> key) (\s@Tag' {} a -> s {key = a} :: Tag)

-- | The name of the group.
tag_resourceId :: Lens.Lens' Tag Prelude.Text
tag_resourceId = Lens.lens (\Tag' {resourceId} -> resourceId) (\s@Tag' {} a -> s {resourceId = a} :: Tag)

-- | The type of resource. The only supported value is @auto-scaling-group@.
tag_resourceType :: Lens.Lens' Tag Prelude.Text
tag_resourceType = Lens.lens (\Tag' {resourceType} -> resourceType) (\s@Tag' {} a -> s {resourceType = a} :: Tag)

-- | Determines whether the tag is added to new instances as they are
-- launched in the group.
tag_propagateAtLaunch :: Lens.Lens' Tag Prelude.Bool
tag_propagateAtLaunch = Lens.lens (\Tag' {propagateAtLaunch} -> propagateAtLaunch) (\s@Tag' {} a -> s {propagateAtLaunch = a} :: Tag)

-- | The tag value.
tag_value :: Lens.Lens' Tag Prelude.Text
tag_value = Lens.lens (\Tag' {value} -> value) (\s@Tag' {} a -> s {value = a} :: Tag)

instance Prelude.Hashable Tag

instance Prelude.NFData Tag

instance Prelude.ToQuery Tag where
  toQuery Tag' {..} =
    Prelude.mconcat
      [ "Key" Prelude.=: key,
        "ResourceId" Prelude.=: resourceId,
        "ResourceType" Prelude.=: resourceType,
        "PropagateAtLaunch" Prelude.=: propagateAtLaunch,
        "Value" Prelude.=: value
      ]
