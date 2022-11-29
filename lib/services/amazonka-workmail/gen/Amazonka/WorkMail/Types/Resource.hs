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
-- Module      : Amazonka.WorkMail.Types.Resource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkMail.Types.Resource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkMail.Types.EntityState
import Amazonka.WorkMail.Types.ResourceType

-- | The representation of a resource.
--
-- /See:/ 'newResource' smart constructor.
data Resource = Resource'
  { -- | The name of the resource.
    name :: Prelude.Maybe Prelude.Text,
    -- | The type of the resource: equipment or room.
    type' :: Prelude.Maybe ResourceType,
    -- | The email of the resource.
    email :: Prelude.Maybe Prelude.Text,
    -- | The state of the resource, which can be ENABLED, DISABLED, or DELETED.
    state :: Prelude.Maybe EntityState,
    -- | The identifier of the resource.
    id :: Prelude.Maybe Prelude.Text,
    -- | The date indicating when the resource was enabled for WorkMail use.
    enabledDate :: Prelude.Maybe Core.POSIX,
    -- | The date indicating when the resource was disabled from WorkMail use.
    disabledDate :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Resource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'resource_name' - The name of the resource.
--
-- 'type'', 'resource_type' - The type of the resource: equipment or room.
--
-- 'email', 'resource_email' - The email of the resource.
--
-- 'state', 'resource_state' - The state of the resource, which can be ENABLED, DISABLED, or DELETED.
--
-- 'id', 'resource_id' - The identifier of the resource.
--
-- 'enabledDate', 'resource_enabledDate' - The date indicating when the resource was enabled for WorkMail use.
--
-- 'disabledDate', 'resource_disabledDate' - The date indicating when the resource was disabled from WorkMail use.
newResource ::
  Resource
newResource =
  Resource'
    { name = Prelude.Nothing,
      type' = Prelude.Nothing,
      email = Prelude.Nothing,
      state = Prelude.Nothing,
      id = Prelude.Nothing,
      enabledDate = Prelude.Nothing,
      disabledDate = Prelude.Nothing
    }

-- | The name of the resource.
resource_name :: Lens.Lens' Resource (Prelude.Maybe Prelude.Text)
resource_name = Lens.lens (\Resource' {name} -> name) (\s@Resource' {} a -> s {name = a} :: Resource)

-- | The type of the resource: equipment or room.
resource_type :: Lens.Lens' Resource (Prelude.Maybe ResourceType)
resource_type = Lens.lens (\Resource' {type'} -> type') (\s@Resource' {} a -> s {type' = a} :: Resource)

-- | The email of the resource.
resource_email :: Lens.Lens' Resource (Prelude.Maybe Prelude.Text)
resource_email = Lens.lens (\Resource' {email} -> email) (\s@Resource' {} a -> s {email = a} :: Resource)

-- | The state of the resource, which can be ENABLED, DISABLED, or DELETED.
resource_state :: Lens.Lens' Resource (Prelude.Maybe EntityState)
resource_state = Lens.lens (\Resource' {state} -> state) (\s@Resource' {} a -> s {state = a} :: Resource)

-- | The identifier of the resource.
resource_id :: Lens.Lens' Resource (Prelude.Maybe Prelude.Text)
resource_id = Lens.lens (\Resource' {id} -> id) (\s@Resource' {} a -> s {id = a} :: Resource)

-- | The date indicating when the resource was enabled for WorkMail use.
resource_enabledDate :: Lens.Lens' Resource (Prelude.Maybe Prelude.UTCTime)
resource_enabledDate = Lens.lens (\Resource' {enabledDate} -> enabledDate) (\s@Resource' {} a -> s {enabledDate = a} :: Resource) Prelude.. Lens.mapping Core._Time

-- | The date indicating when the resource was disabled from WorkMail use.
resource_disabledDate :: Lens.Lens' Resource (Prelude.Maybe Prelude.UTCTime)
resource_disabledDate = Lens.lens (\Resource' {disabledDate} -> disabledDate) (\s@Resource' {} a -> s {disabledDate = a} :: Resource) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON Resource where
  parseJSON =
    Core.withObject
      "Resource"
      ( \x ->
          Resource'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "Email")
            Prelude.<*> (x Core..:? "State")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "EnabledDate")
            Prelude.<*> (x Core..:? "DisabledDate")
      )

instance Prelude.Hashable Resource where
  hashWithSalt _salt Resource' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` email
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` enabledDate
      `Prelude.hashWithSalt` disabledDate

instance Prelude.NFData Resource where
  rnf Resource' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf email
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf enabledDate
      `Prelude.seq` Prelude.rnf disabledDate
