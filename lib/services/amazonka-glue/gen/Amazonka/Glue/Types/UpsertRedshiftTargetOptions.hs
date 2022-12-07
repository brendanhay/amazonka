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
-- Module      : Amazonka.Glue.Types.UpsertRedshiftTargetOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.UpsertRedshiftTargetOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The options to configure an upsert operation when writing to a Redshift
-- target .
--
-- /See:/ 'newUpsertRedshiftTargetOptions' smart constructor.
data UpsertRedshiftTargetOptions = UpsertRedshiftTargetOptions'
  { -- | The physical location of the Redshift table.
    tableLocation :: Prelude.Maybe Prelude.Text,
    -- | The keys used to determine whether to perform an update or insert.
    upsertKeys :: Prelude.Maybe [Prelude.Text],
    -- | The name of the connection to use to write to Redshift.
    connectionName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpsertRedshiftTargetOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tableLocation', 'upsertRedshiftTargetOptions_tableLocation' - The physical location of the Redshift table.
--
-- 'upsertKeys', 'upsertRedshiftTargetOptions_upsertKeys' - The keys used to determine whether to perform an update or insert.
--
-- 'connectionName', 'upsertRedshiftTargetOptions_connectionName' - The name of the connection to use to write to Redshift.
newUpsertRedshiftTargetOptions ::
  UpsertRedshiftTargetOptions
newUpsertRedshiftTargetOptions =
  UpsertRedshiftTargetOptions'
    { tableLocation =
        Prelude.Nothing,
      upsertKeys = Prelude.Nothing,
      connectionName = Prelude.Nothing
    }

-- | The physical location of the Redshift table.
upsertRedshiftTargetOptions_tableLocation :: Lens.Lens' UpsertRedshiftTargetOptions (Prelude.Maybe Prelude.Text)
upsertRedshiftTargetOptions_tableLocation = Lens.lens (\UpsertRedshiftTargetOptions' {tableLocation} -> tableLocation) (\s@UpsertRedshiftTargetOptions' {} a -> s {tableLocation = a} :: UpsertRedshiftTargetOptions)

-- | The keys used to determine whether to perform an update or insert.
upsertRedshiftTargetOptions_upsertKeys :: Lens.Lens' UpsertRedshiftTargetOptions (Prelude.Maybe [Prelude.Text])
upsertRedshiftTargetOptions_upsertKeys = Lens.lens (\UpsertRedshiftTargetOptions' {upsertKeys} -> upsertKeys) (\s@UpsertRedshiftTargetOptions' {} a -> s {upsertKeys = a} :: UpsertRedshiftTargetOptions) Prelude.. Lens.mapping Lens.coerced

-- | The name of the connection to use to write to Redshift.
upsertRedshiftTargetOptions_connectionName :: Lens.Lens' UpsertRedshiftTargetOptions (Prelude.Maybe Prelude.Text)
upsertRedshiftTargetOptions_connectionName = Lens.lens (\UpsertRedshiftTargetOptions' {connectionName} -> connectionName) (\s@UpsertRedshiftTargetOptions' {} a -> s {connectionName = a} :: UpsertRedshiftTargetOptions)

instance Data.FromJSON UpsertRedshiftTargetOptions where
  parseJSON =
    Data.withObject
      "UpsertRedshiftTargetOptions"
      ( \x ->
          UpsertRedshiftTargetOptions'
            Prelude.<$> (x Data..:? "TableLocation")
            Prelude.<*> (x Data..:? "UpsertKeys" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ConnectionName")
      )

instance Prelude.Hashable UpsertRedshiftTargetOptions where
  hashWithSalt _salt UpsertRedshiftTargetOptions' {..} =
    _salt `Prelude.hashWithSalt` tableLocation
      `Prelude.hashWithSalt` upsertKeys
      `Prelude.hashWithSalt` connectionName

instance Prelude.NFData UpsertRedshiftTargetOptions where
  rnf UpsertRedshiftTargetOptions' {..} =
    Prelude.rnf tableLocation
      `Prelude.seq` Prelude.rnf upsertKeys
      `Prelude.seq` Prelude.rnf connectionName

instance Data.ToJSON UpsertRedshiftTargetOptions where
  toJSON UpsertRedshiftTargetOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("TableLocation" Data..=) Prelude.<$> tableLocation,
            ("UpsertKeys" Data..=) Prelude.<$> upsertKeys,
            ("ConnectionName" Data..=)
              Prelude.<$> connectionName
          ]
      )
