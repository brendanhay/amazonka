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
-- Module      : Network.AWS.Mobile.Types.Resource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Mobile.Types.Resource where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about an instance of an AWS resource associated with a
-- project.
--
-- /See:/ 'newResource' smart constructor.
data Resource = Resource'
  { arn :: Core.Maybe Core.Text,
    name :: Core.Maybe Core.Text,
    attributes :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    feature :: Core.Maybe Core.Text,
    type' :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Resource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'resource_arn' - Undocumented member.
--
-- 'name', 'resource_name' - Undocumented member.
--
-- 'attributes', 'resource_attributes' - Undocumented member.
--
-- 'feature', 'resource_feature' - Undocumented member.
--
-- 'type'', 'resource_type' - Undocumented member.
newResource ::
  Resource
newResource =
  Resource'
    { arn = Core.Nothing,
      name = Core.Nothing,
      attributes = Core.Nothing,
      feature = Core.Nothing,
      type' = Core.Nothing
    }

-- | Undocumented member.
resource_arn :: Lens.Lens' Resource (Core.Maybe Core.Text)
resource_arn = Lens.lens (\Resource' {arn} -> arn) (\s@Resource' {} a -> s {arn = a} :: Resource)

-- | Undocumented member.
resource_name :: Lens.Lens' Resource (Core.Maybe Core.Text)
resource_name = Lens.lens (\Resource' {name} -> name) (\s@Resource' {} a -> s {name = a} :: Resource)

-- | Undocumented member.
resource_attributes :: Lens.Lens' Resource (Core.Maybe (Core.HashMap Core.Text Core.Text))
resource_attributes = Lens.lens (\Resource' {attributes} -> attributes) (\s@Resource' {} a -> s {attributes = a} :: Resource) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
resource_feature :: Lens.Lens' Resource (Core.Maybe Core.Text)
resource_feature = Lens.lens (\Resource' {feature} -> feature) (\s@Resource' {} a -> s {feature = a} :: Resource)

-- | Undocumented member.
resource_type :: Lens.Lens' Resource (Core.Maybe Core.Text)
resource_type = Lens.lens (\Resource' {type'} -> type') (\s@Resource' {} a -> s {type' = a} :: Resource)

instance Core.FromJSON Resource where
  parseJSON =
    Core.withObject
      "Resource"
      ( \x ->
          Resource'
            Core.<$> (x Core..:? "arn")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "attributes" Core..!= Core.mempty)
            Core.<*> (x Core..:? "feature")
            Core.<*> (x Core..:? "type")
      )

instance Core.Hashable Resource

instance Core.NFData Resource
