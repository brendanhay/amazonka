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
-- Module      : Amazonka.GamesParks.Types.ImportGameConfigurationSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GamesParks.Types.ImportGameConfigurationSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The source used to import configuration sections.
--
-- /See:/ 'newImportGameConfigurationSource' smart constructor.
data ImportGameConfigurationSource = ImportGameConfigurationSource'
  { -- | The JSON string containing the configuration sections.
    file :: Data.Base64
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportGameConfigurationSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'file', 'importGameConfigurationSource_file' - The JSON string containing the configuration sections.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
newImportGameConfigurationSource ::
  -- | 'file'
  Prelude.ByteString ->
  ImportGameConfigurationSource
newImportGameConfigurationSource pFile_ =
  ImportGameConfigurationSource'
    { file =
        Data._Base64 Lens.# pFile_
    }

-- | The JSON string containing the configuration sections.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
importGameConfigurationSource_file :: Lens.Lens' ImportGameConfigurationSource Prelude.ByteString
importGameConfigurationSource_file = Lens.lens (\ImportGameConfigurationSource' {file} -> file) (\s@ImportGameConfigurationSource' {} a -> s {file = a} :: ImportGameConfigurationSource) Prelude.. Data._Base64

instance
  Prelude.Hashable
    ImportGameConfigurationSource
  where
  hashWithSalt _salt ImportGameConfigurationSource' {..} =
    _salt `Prelude.hashWithSalt` file

instance Prelude.NFData ImportGameConfigurationSource where
  rnf ImportGameConfigurationSource' {..} =
    Prelude.rnf file

instance Data.ToJSON ImportGameConfigurationSource where
  toJSON ImportGameConfigurationSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("File" Data..= file)]
      )
