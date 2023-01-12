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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.ControlMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The metadata that\'s associated with the standard control or custom
-- control.
--
-- /See:/ 'newControlMetadata' smart constructor.
data ControlMetadata = ControlMetadata'
  { -- | The Amazon Resource Name (ARN) of the control.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The data source that determines where Audit Manager collects evidence
    -- from for the control.
    controlSources :: Prelude.Maybe Prelude.Text,
    -- | The time when the control was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The unique identifier for the control.
    id :: Prelude.Maybe Prelude.Text,
    -- | The time when the control was most recently updated.
    lastUpdatedAt :: Prelude.Maybe Data.POSIX,
    -- | The name of the control.
    name :: Prelude.Maybe Prelude.Text
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
-- 'arn', 'controlMetadata_arn' - The Amazon Resource Name (ARN) of the control.
--
-- 'controlSources', 'controlMetadata_controlSources' - The data source that determines where Audit Manager collects evidence
-- from for the control.
--
-- 'createdAt', 'controlMetadata_createdAt' - The time when the control was created.
--
-- 'id', 'controlMetadata_id' - The unique identifier for the control.
--
-- 'lastUpdatedAt', 'controlMetadata_lastUpdatedAt' - The time when the control was most recently updated.
--
-- 'name', 'controlMetadata_name' - The name of the control.
newControlMetadata ::
  ControlMetadata
newControlMetadata =
  ControlMetadata'
    { arn = Prelude.Nothing,
      controlSources = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      id = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the control.
controlMetadata_arn :: Lens.Lens' ControlMetadata (Prelude.Maybe Prelude.Text)
controlMetadata_arn = Lens.lens (\ControlMetadata' {arn} -> arn) (\s@ControlMetadata' {} a -> s {arn = a} :: ControlMetadata)

-- | The data source that determines where Audit Manager collects evidence
-- from for the control.
controlMetadata_controlSources :: Lens.Lens' ControlMetadata (Prelude.Maybe Prelude.Text)
controlMetadata_controlSources = Lens.lens (\ControlMetadata' {controlSources} -> controlSources) (\s@ControlMetadata' {} a -> s {controlSources = a} :: ControlMetadata)

-- | The time when the control was created.
controlMetadata_createdAt :: Lens.Lens' ControlMetadata (Prelude.Maybe Prelude.UTCTime)
controlMetadata_createdAt = Lens.lens (\ControlMetadata' {createdAt} -> createdAt) (\s@ControlMetadata' {} a -> s {createdAt = a} :: ControlMetadata) Prelude.. Lens.mapping Data._Time

-- | The unique identifier for the control.
controlMetadata_id :: Lens.Lens' ControlMetadata (Prelude.Maybe Prelude.Text)
controlMetadata_id = Lens.lens (\ControlMetadata' {id} -> id) (\s@ControlMetadata' {} a -> s {id = a} :: ControlMetadata)

-- | The time when the control was most recently updated.
controlMetadata_lastUpdatedAt :: Lens.Lens' ControlMetadata (Prelude.Maybe Prelude.UTCTime)
controlMetadata_lastUpdatedAt = Lens.lens (\ControlMetadata' {lastUpdatedAt} -> lastUpdatedAt) (\s@ControlMetadata' {} a -> s {lastUpdatedAt = a} :: ControlMetadata) Prelude.. Lens.mapping Data._Time

-- | The name of the control.
controlMetadata_name :: Lens.Lens' ControlMetadata (Prelude.Maybe Prelude.Text)
controlMetadata_name = Lens.lens (\ControlMetadata' {name} -> name) (\s@ControlMetadata' {} a -> s {name = a} :: ControlMetadata)

instance Data.FromJSON ControlMetadata where
  parseJSON =
    Data.withObject
      "ControlMetadata"
      ( \x ->
          ControlMetadata'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "controlSources")
            Prelude.<*> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "lastUpdatedAt")
            Prelude.<*> (x Data..:? "name")
      )

instance Prelude.Hashable ControlMetadata where
  hashWithSalt _salt ControlMetadata' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` controlSources
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` lastUpdatedAt
      `Prelude.hashWithSalt` name

instance Prelude.NFData ControlMetadata where
  rnf ControlMetadata' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf controlSources
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf name
