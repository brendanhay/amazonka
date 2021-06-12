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
-- Module      : Network.AWS.Route53.Types.Tag
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.Tag where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Route53.Internal

-- | A complex type that contains information about a tag that you want to
-- add or edit for the specified health check or hosted zone.
--
-- /See:/ 'newTag' smart constructor.
data Tag = Tag'
  { -- | The value of @Key@ depends on the operation that you want to perform:
    --
    -- -   __Add a tag to a health check or hosted zone__: @Key@ is the name
    --     that you want to give the new tag.
    --
    -- -   __Edit a tag__: @Key@ is the name of the tag that you want to change
    --     the @Value@ for.
    --
    -- -   __Delete a key__: @Key@ is the name of the tag you want to remove.
    --
    -- -   __Give a name to a health check__: Edit the default @Name@ tag. In
    --     the Amazon Route 53 console, the list of your health checks includes
    --     a __Name__ column that lets you see the name that you\'ve given to
    --     each health check.
    key :: Core.Maybe Core.Text,
    -- | The value of @Value@ depends on the operation that you want to perform:
    --
    -- -   __Add a tag to a health check or hosted zone__: @Value@ is the value
    --     that you want to give the new tag.
    --
    -- -   __Edit a tag__: @Value@ is the new value that you want to assign the
    --     tag.
    value :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Tag' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'tag_key' - The value of @Key@ depends on the operation that you want to perform:
--
-- -   __Add a tag to a health check or hosted zone__: @Key@ is the name
--     that you want to give the new tag.
--
-- -   __Edit a tag__: @Key@ is the name of the tag that you want to change
--     the @Value@ for.
--
-- -   __Delete a key__: @Key@ is the name of the tag you want to remove.
--
-- -   __Give a name to a health check__: Edit the default @Name@ tag. In
--     the Amazon Route 53 console, the list of your health checks includes
--     a __Name__ column that lets you see the name that you\'ve given to
--     each health check.
--
-- 'value', 'tag_value' - The value of @Value@ depends on the operation that you want to perform:
--
-- -   __Add a tag to a health check or hosted zone__: @Value@ is the value
--     that you want to give the new tag.
--
-- -   __Edit a tag__: @Value@ is the new value that you want to assign the
--     tag.
newTag ::
  Tag
newTag =
  Tag' {key = Core.Nothing, value = Core.Nothing}

-- | The value of @Key@ depends on the operation that you want to perform:
--
-- -   __Add a tag to a health check or hosted zone__: @Key@ is the name
--     that you want to give the new tag.
--
-- -   __Edit a tag__: @Key@ is the name of the tag that you want to change
--     the @Value@ for.
--
-- -   __Delete a key__: @Key@ is the name of the tag you want to remove.
--
-- -   __Give a name to a health check__: Edit the default @Name@ tag. In
--     the Amazon Route 53 console, the list of your health checks includes
--     a __Name__ column that lets you see the name that you\'ve given to
--     each health check.
tag_key :: Lens.Lens' Tag (Core.Maybe Core.Text)
tag_key = Lens.lens (\Tag' {key} -> key) (\s@Tag' {} a -> s {key = a} :: Tag)

-- | The value of @Value@ depends on the operation that you want to perform:
--
-- -   __Add a tag to a health check or hosted zone__: @Value@ is the value
--     that you want to give the new tag.
--
-- -   __Edit a tag__: @Value@ is the new value that you want to assign the
--     tag.
tag_value :: Lens.Lens' Tag (Core.Maybe Core.Text)
tag_value = Lens.lens (\Tag' {value} -> value) (\s@Tag' {} a -> s {value = a} :: Tag)

instance Core.FromXML Tag where
  parseXML x =
    Tag'
      Core.<$> (x Core..@? "Key") Core.<*> (x Core..@? "Value")

instance Core.Hashable Tag

instance Core.NFData Tag

instance Core.ToXML Tag where
  toXML Tag' {..} =
    Core.mconcat
      ["Key" Core.@= key, "Value" Core.@= value]
