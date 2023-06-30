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
-- Module      : Amazonka.EMRServerless.Types.Hive
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMRServerless.Types.Hive where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configurations for the Hive job driver.
--
-- /See:/ 'newHive' smart constructor.
data Hive = Hive'
  { -- | The query file for the Hive job run.
    initQueryFile :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The parameters for the Hive job run.
    parameters :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The query for the Hive job run.
    query :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Hive' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'initQueryFile', 'hive_initQueryFile' - The query file for the Hive job run.
--
-- 'parameters', 'hive_parameters' - The parameters for the Hive job run.
--
-- 'query', 'hive_query' - The query for the Hive job run.
newHive ::
  -- | 'query'
  Prelude.Text ->
  Hive
newHive pQuery_ =
  Hive'
    { initQueryFile = Prelude.Nothing,
      parameters = Prelude.Nothing,
      query = Data._Sensitive Lens.# pQuery_
    }

-- | The query file for the Hive job run.
hive_initQueryFile :: Lens.Lens' Hive (Prelude.Maybe Prelude.Text)
hive_initQueryFile = Lens.lens (\Hive' {initQueryFile} -> initQueryFile) (\s@Hive' {} a -> s {initQueryFile = a} :: Hive) Prelude.. Lens.mapping Data._Sensitive

-- | The parameters for the Hive job run.
hive_parameters :: Lens.Lens' Hive (Prelude.Maybe Prelude.Text)
hive_parameters = Lens.lens (\Hive' {parameters} -> parameters) (\s@Hive' {} a -> s {parameters = a} :: Hive) Prelude.. Lens.mapping Data._Sensitive

-- | The query for the Hive job run.
hive_query :: Lens.Lens' Hive Prelude.Text
hive_query = Lens.lens (\Hive' {query} -> query) (\s@Hive' {} a -> s {query = a} :: Hive) Prelude.. Data._Sensitive

instance Data.FromJSON Hive where
  parseJSON =
    Data.withObject
      "Hive"
      ( \x ->
          Hive'
            Prelude.<$> (x Data..:? "initQueryFile")
            Prelude.<*> (x Data..:? "parameters")
            Prelude.<*> (x Data..: "query")
      )

instance Prelude.Hashable Hive where
  hashWithSalt _salt Hive' {..} =
    _salt
      `Prelude.hashWithSalt` initQueryFile
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` query

instance Prelude.NFData Hive where
  rnf Hive' {..} =
    Prelude.rnf initQueryFile
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf query

instance Data.ToJSON Hive where
  toJSON Hive' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("initQueryFile" Data..=) Prelude.<$> initQueryFile,
            ("parameters" Data..=) Prelude.<$> parameters,
            Prelude.Just ("query" Data..= query)
          ]
      )
