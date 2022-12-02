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
-- Module      : Amazonka.Glue.Types.DeltaTarget
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.DeltaTarget where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies a Delta data store to crawl one or more Delta tables.
--
-- /See:/ 'newDeltaTarget' smart constructor.
data DeltaTarget = DeltaTarget'
  { -- | A list of the Amazon S3 paths to the Delta tables.
    deltaTables :: Prelude.Maybe [Prelude.Text],
    -- | Specifies whether to write the manifest files to the Delta table path.
    writeManifest :: Prelude.Maybe Prelude.Bool,
    -- | The name of the connection to use to connect to the Delta table target.
    connectionName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeltaTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deltaTables', 'deltaTarget_deltaTables' - A list of the Amazon S3 paths to the Delta tables.
--
-- 'writeManifest', 'deltaTarget_writeManifest' - Specifies whether to write the manifest files to the Delta table path.
--
-- 'connectionName', 'deltaTarget_connectionName' - The name of the connection to use to connect to the Delta table target.
newDeltaTarget ::
  DeltaTarget
newDeltaTarget =
  DeltaTarget'
    { deltaTables = Prelude.Nothing,
      writeManifest = Prelude.Nothing,
      connectionName = Prelude.Nothing
    }

-- | A list of the Amazon S3 paths to the Delta tables.
deltaTarget_deltaTables :: Lens.Lens' DeltaTarget (Prelude.Maybe [Prelude.Text])
deltaTarget_deltaTables = Lens.lens (\DeltaTarget' {deltaTables} -> deltaTables) (\s@DeltaTarget' {} a -> s {deltaTables = a} :: DeltaTarget) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether to write the manifest files to the Delta table path.
deltaTarget_writeManifest :: Lens.Lens' DeltaTarget (Prelude.Maybe Prelude.Bool)
deltaTarget_writeManifest = Lens.lens (\DeltaTarget' {writeManifest} -> writeManifest) (\s@DeltaTarget' {} a -> s {writeManifest = a} :: DeltaTarget)

-- | The name of the connection to use to connect to the Delta table target.
deltaTarget_connectionName :: Lens.Lens' DeltaTarget (Prelude.Maybe Prelude.Text)
deltaTarget_connectionName = Lens.lens (\DeltaTarget' {connectionName} -> connectionName) (\s@DeltaTarget' {} a -> s {connectionName = a} :: DeltaTarget)

instance Data.FromJSON DeltaTarget where
  parseJSON =
    Data.withObject
      "DeltaTarget"
      ( \x ->
          DeltaTarget'
            Prelude.<$> (x Data..:? "DeltaTables" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "WriteManifest")
            Prelude.<*> (x Data..:? "ConnectionName")
      )

instance Prelude.Hashable DeltaTarget where
  hashWithSalt _salt DeltaTarget' {..} =
    _salt `Prelude.hashWithSalt` deltaTables
      `Prelude.hashWithSalt` writeManifest
      `Prelude.hashWithSalt` connectionName

instance Prelude.NFData DeltaTarget where
  rnf DeltaTarget' {..} =
    Prelude.rnf deltaTables
      `Prelude.seq` Prelude.rnf writeManifest
      `Prelude.seq` Prelude.rnf connectionName

instance Data.ToJSON DeltaTarget where
  toJSON DeltaTarget' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DeltaTables" Data..=) Prelude.<$> deltaTables,
            ("WriteManifest" Data..=) Prelude.<$> writeManifest,
            ("ConnectionName" Data..=)
              Prelude.<$> connectionName
          ]
      )
