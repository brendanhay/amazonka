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
-- Module      : Amazonka.AuditManager.Types.ControlMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.ControlMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The metadata that\'s associated with the standard control or custom
-- control.
--
-- /See:/ 'newControlMetadata' smart constructor.
data ControlMetadata = ControlMetadata'
  { -- | The name of the control.
    name :: Prelude.Maybe Prelude.Text,
    -- | Specifies when the control was most recently updated.
    lastUpdatedAt :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the control.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the control.
    id :: Prelude.Maybe Prelude.Text,
    -- | Specifies when the control was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The data source that determines where Audit Manager collects evidence
    -- from for the control.
    controlSources :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ControlMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'controlMetadata_name' - The name of the control.
--
-- 'lastUpdatedAt', 'controlMetadata_lastUpdatedAt' - Specifies when the control was most recently updated.
--
-- 'arn', 'controlMetadata_arn' - The Amazon Resource Name (ARN) of the control.
--
-- 'id', 'controlMetadata_id' - The unique identifier for the control.
--
-- 'createdAt', 'controlMetadata_createdAt' - Specifies when the control was created.
--
-- 'controlSources', 'controlMetadata_controlSources' - The data source that determines where Audit Manager collects evidence
-- from for the control.
newControlMetadata ::
  ControlMetadata
newControlMetadata =
  ControlMetadata'
    { name = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      controlSources = Prelude.Nothing
    }

-- | The name of the control.
controlMetadata_name :: Lens.Lens' ControlMetadata (Prelude.Maybe Prelude.Text)
controlMetadata_name = Lens.lens (\ControlMetadata' {name} -> name) (\s@ControlMetadata' {} a -> s {name = a} :: ControlMetadata)

-- | Specifies when the control was most recently updated.
controlMetadata_lastUpdatedAt :: Lens.Lens' ControlMetadata (Prelude.Maybe Prelude.UTCTime)
controlMetadata_lastUpdatedAt = Lens.lens (\ControlMetadata' {lastUpdatedAt} -> lastUpdatedAt) (\s@ControlMetadata' {} a -> s {lastUpdatedAt = a} :: ControlMetadata) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the control.
controlMetadata_arn :: Lens.Lens' ControlMetadata (Prelude.Maybe Prelude.Text)
controlMetadata_arn = Lens.lens (\ControlMetadata' {arn} -> arn) (\s@ControlMetadata' {} a -> s {arn = a} :: ControlMetadata)

-- | The unique identifier for the control.
controlMetadata_id :: Lens.Lens' ControlMetadata (Prelude.Maybe Prelude.Text)
controlMetadata_id = Lens.lens (\ControlMetadata' {id} -> id) (\s@ControlMetadata' {} a -> s {id = a} :: ControlMetadata)

-- | Specifies when the control was created.
controlMetadata_createdAt :: Lens.Lens' ControlMetadata (Prelude.Maybe Prelude.UTCTime)
controlMetadata_createdAt = Lens.lens (\ControlMetadata' {createdAt} -> createdAt) (\s@ControlMetadata' {} a -> s {createdAt = a} :: ControlMetadata) Prelude.. Lens.mapping Core._Time

-- | The data source that determines where Audit Manager collects evidence
-- from for the control.
controlMetadata_controlSources :: Lens.Lens' ControlMetadata (Prelude.Maybe Prelude.Text)
controlMetadata_controlSources = Lens.lens (\ControlMetadata' {controlSources} -> controlSources) (\s@ControlMetadata' {} a -> s {controlSources = a} :: ControlMetadata)

instance Core.FromJSON ControlMetadata where
  parseJSON =
    Core.withObject
      "ControlMetadata"
      ( \x ->
          ControlMetadata'
            Prelude.<$> (x Core..:? "name")
            Prelude.<*> (x Core..:? "lastUpdatedAt")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "controlSources")
      )

instance Prelude.Hashable ControlMetadata where
  hashWithSalt _salt ControlMetadata' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` lastUpdatedAt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` controlSources

instance Prelude.NFData ControlMetadata where
  rnf ControlMetadata' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf controlSources
