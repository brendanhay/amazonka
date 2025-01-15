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
-- Module      : Amazonka.AutoScaling.Types.Tag
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.Tag where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a tag for an Auto Scaling group.
--
-- /See:/ 'newTag' smart constructor.
data Tag = Tag'
  { -- | The tag key.
    key :: Prelude.Text,
    -- | The name of the Auto Scaling group.
    resourceId :: Prelude.Text,
    -- | The type of resource. The only supported value is @auto-scaling-group@.
    resourceType :: Prelude.Text,
    -- | Determines whether the tag is added to new instances as they are
    -- launched in the group.
    propagateAtLaunch :: Prelude.Bool,
    -- | The tag value.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'resourceId', 'tag_resourceId' - The name of the Auto Scaling group.
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

-- | The name of the Auto Scaling group.
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

instance Prelude.Hashable Tag where
  hashWithSalt _salt Tag' {..} =
    _salt
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` propagateAtLaunch
      `Prelude.hashWithSalt` value

instance Prelude.NFData Tag where
  rnf Tag' {..} =
    Prelude.rnf key `Prelude.seq`
      Prelude.rnf resourceId `Prelude.seq`
        Prelude.rnf resourceType `Prelude.seq`
          Prelude.rnf propagateAtLaunch `Prelude.seq`
            Prelude.rnf value

instance Data.ToQuery Tag where
  toQuery Tag' {..} =
    Prelude.mconcat
      [ "Key" Data.=: key,
        "ResourceId" Data.=: resourceId,
        "ResourceType" Data.=: resourceType,
        "PropagateAtLaunch" Data.=: propagateAtLaunch,
        "Value" Data.=: value
      ]
