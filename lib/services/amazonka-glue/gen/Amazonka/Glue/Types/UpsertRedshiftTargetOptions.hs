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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
  { -- | The name of the connection to use to write to Redshift.
    connectionName :: Prelude.Maybe Prelude.Text,
    -- | The physical location of the Redshift table.
    tableLocation :: Prelude.Maybe Prelude.Text,
    -- | The keys used to determine whether to perform an update or insert.
    upsertKeys :: Prelude.Maybe [Prelude.Text]
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
-- 'connectionName', 'upsertRedshiftTargetOptions_connectionName' - The name of the connection to use to write to Redshift.
--
-- 'tableLocation', 'upsertRedshiftTargetOptions_tableLocation' - The physical location of the Redshift table.
--
-- 'upsertKeys', 'upsertRedshiftTargetOptions_upsertKeys' - The keys used to determine whether to perform an update or insert.
newUpsertRedshiftTargetOptions ::
  UpsertRedshiftTargetOptions
newUpsertRedshiftTargetOptions =
  UpsertRedshiftTargetOptions'
    { connectionName =
        Prelude.Nothing,
      tableLocation = Prelude.Nothing,
      upsertKeys = Prelude.Nothing
    }

-- | The name of the connection to use to write to Redshift.
upsertRedshiftTargetOptions_connectionName :: Lens.Lens' UpsertRedshiftTargetOptions (Prelude.Maybe Prelude.Text)
upsertRedshiftTargetOptions_connectionName = Lens.lens (\UpsertRedshiftTargetOptions' {connectionName} -> connectionName) (\s@UpsertRedshiftTargetOptions' {} a -> s {connectionName = a} :: UpsertRedshiftTargetOptions)

-- | The physical location of the Redshift table.
upsertRedshiftTargetOptions_tableLocation :: Lens.Lens' UpsertRedshiftTargetOptions (Prelude.Maybe Prelude.Text)
upsertRedshiftTargetOptions_tableLocation = Lens.lens (\UpsertRedshiftTargetOptions' {tableLocation} -> tableLocation) (\s@UpsertRedshiftTargetOptions' {} a -> s {tableLocation = a} :: UpsertRedshiftTargetOptions)

-- | The keys used to determine whether to perform an update or insert.
upsertRedshiftTargetOptions_upsertKeys :: Lens.Lens' UpsertRedshiftTargetOptions (Prelude.Maybe [Prelude.Text])
upsertRedshiftTargetOptions_upsertKeys = Lens.lens (\UpsertRedshiftTargetOptions' {upsertKeys} -> upsertKeys) (\s@UpsertRedshiftTargetOptions' {} a -> s {upsertKeys = a} :: UpsertRedshiftTargetOptions) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON UpsertRedshiftTargetOptions where
  parseJSON =
    Data.withObject
      "UpsertRedshiftTargetOptions"
      ( \x ->
          UpsertRedshiftTargetOptions'
            Prelude.<$> (x Data..:? "ConnectionName")
            Prelude.<*> (x Data..:? "TableLocation")
            Prelude.<*> (x Data..:? "UpsertKeys" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable UpsertRedshiftTargetOptions where
  hashWithSalt _salt UpsertRedshiftTargetOptions' {..} =
    _salt
      `Prelude.hashWithSalt` connectionName
      `Prelude.hashWithSalt` tableLocation
      `Prelude.hashWithSalt` upsertKeys

instance Prelude.NFData UpsertRedshiftTargetOptions where
  rnf UpsertRedshiftTargetOptions' {..} =
    Prelude.rnf connectionName `Prelude.seq`
      Prelude.rnf tableLocation `Prelude.seq`
        Prelude.rnf upsertKeys

instance Data.ToJSON UpsertRedshiftTargetOptions where
  toJSON UpsertRedshiftTargetOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ConnectionName" Data..=)
              Prelude.<$> connectionName,
            ("TableLocation" Data..=) Prelude.<$> tableLocation,
            ("UpsertKeys" Data..=) Prelude.<$> upsertKeys
          ]
      )
