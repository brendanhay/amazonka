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
-- Module      : Network.AWS.WorkMail.Types.Resource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkMail.Types.Resource where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.WorkMail.Types.EntityState
import Network.AWS.WorkMail.Types.ResourceType

-- | The representation of a resource.
--
-- /See:/ 'newResource' smart constructor.
data Resource = Resource'
  { -- | The date indicating when the resource was enabled for Amazon WorkMail
    -- use.
    enabledDate :: Core.Maybe Core.POSIX,
    -- | The identifier of the resource.
    id :: Core.Maybe Core.Text,
    -- | The state of the resource, which can be ENABLED, DISABLED, or DELETED.
    state :: Core.Maybe EntityState,
    -- | The name of the resource.
    name :: Core.Maybe Core.Text,
    -- | The email of the resource.
    email :: Core.Maybe Core.Text,
    -- | The date indicating when the resource was disabled from Amazon WorkMail
    -- use.
    disabledDate :: Core.Maybe Core.POSIX,
    -- | The type of the resource: equipment or room.
    type' :: Core.Maybe ResourceType
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
-- 'enabledDate', 'resource_enabledDate' - The date indicating when the resource was enabled for Amazon WorkMail
-- use.
--
-- 'id', 'resource_id' - The identifier of the resource.
--
-- 'state', 'resource_state' - The state of the resource, which can be ENABLED, DISABLED, or DELETED.
--
-- 'name', 'resource_name' - The name of the resource.
--
-- 'email', 'resource_email' - The email of the resource.
--
-- 'disabledDate', 'resource_disabledDate' - The date indicating when the resource was disabled from Amazon WorkMail
-- use.
--
-- 'type'', 'resource_type' - The type of the resource: equipment or room.
newResource ::
  Resource
newResource =
  Resource'
    { enabledDate = Core.Nothing,
      id = Core.Nothing,
      state = Core.Nothing,
      name = Core.Nothing,
      email = Core.Nothing,
      disabledDate = Core.Nothing,
      type' = Core.Nothing
    }

-- | The date indicating when the resource was enabled for Amazon WorkMail
-- use.
resource_enabledDate :: Lens.Lens' Resource (Core.Maybe Core.UTCTime)
resource_enabledDate = Lens.lens (\Resource' {enabledDate} -> enabledDate) (\s@Resource' {} a -> s {enabledDate = a} :: Resource) Core.. Lens.mapping Core._Time

-- | The identifier of the resource.
resource_id :: Lens.Lens' Resource (Core.Maybe Core.Text)
resource_id = Lens.lens (\Resource' {id} -> id) (\s@Resource' {} a -> s {id = a} :: Resource)

-- | The state of the resource, which can be ENABLED, DISABLED, or DELETED.
resource_state :: Lens.Lens' Resource (Core.Maybe EntityState)
resource_state = Lens.lens (\Resource' {state} -> state) (\s@Resource' {} a -> s {state = a} :: Resource)

-- | The name of the resource.
resource_name :: Lens.Lens' Resource (Core.Maybe Core.Text)
resource_name = Lens.lens (\Resource' {name} -> name) (\s@Resource' {} a -> s {name = a} :: Resource)

-- | The email of the resource.
resource_email :: Lens.Lens' Resource (Core.Maybe Core.Text)
resource_email = Lens.lens (\Resource' {email} -> email) (\s@Resource' {} a -> s {email = a} :: Resource)

-- | The date indicating when the resource was disabled from Amazon WorkMail
-- use.
resource_disabledDate :: Lens.Lens' Resource (Core.Maybe Core.UTCTime)
resource_disabledDate = Lens.lens (\Resource' {disabledDate} -> disabledDate) (\s@Resource' {} a -> s {disabledDate = a} :: Resource) Core.. Lens.mapping Core._Time

-- | The type of the resource: equipment or room.
resource_type :: Lens.Lens' Resource (Core.Maybe ResourceType)
resource_type = Lens.lens (\Resource' {type'} -> type') (\s@Resource' {} a -> s {type' = a} :: Resource)

instance Core.FromJSON Resource where
  parseJSON =
    Core.withObject
      "Resource"
      ( \x ->
          Resource'
            Core.<$> (x Core..:? "EnabledDate")
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "State")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "Email")
            Core.<*> (x Core..:? "DisabledDate")
            Core.<*> (x Core..:? "Type")
      )

instance Core.Hashable Resource

instance Core.NFData Resource
