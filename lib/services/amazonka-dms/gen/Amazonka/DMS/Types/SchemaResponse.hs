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
-- Module      : Amazonka.DMS.Types.SchemaResponse
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.SchemaResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types.DatabaseShortInfoResponse
import Amazonka.DMS.Types.SchemaShortInfoResponse
import Amazonka.DMS.Types.ServerShortInfoResponse
import qualified Amazonka.Prelude as Prelude

-- | Describes a schema in a Fleet Advisor collector inventory.
--
-- /See:/ 'newSchemaResponse' smart constructor.
data SchemaResponse = SchemaResponse'
  { -- | The database for a schema in a Fleet Advisor collector inventory.
    databaseInstance :: Prelude.Maybe DatabaseShortInfoResponse,
    -- | The similarity value for a schema in a Fleet Advisor collector
    -- inventory. A higher similarity value indicates that a schema is likely
    -- to be a duplicate.
    similarity :: Prelude.Maybe Prelude.Double,
    originalSchema :: Prelude.Maybe SchemaShortInfoResponse,
    -- | The number of lines of code in a schema in a Fleet Advisor collector
    -- inventory.
    codeLineCount :: Prelude.Maybe Prelude.Integer,
    -- | The name of a schema in a Fleet Advisor collector inventory.
    schemaName :: Prelude.Maybe Prelude.Text,
    -- | The database server for a schema in a Fleet Advisor collector inventory.
    server :: Prelude.Maybe ServerShortInfoResponse,
    -- | The complexity level of the code in a schema in a Fleet Advisor
    -- collector inventory.
    complexity :: Prelude.Maybe Prelude.Text,
    -- | The ID of a schema in a Fleet Advisor collector inventory.
    schemaId :: Prelude.Maybe Prelude.Text,
    -- | The size level of the code in a schema in a Fleet Advisor collector
    -- inventory.
    codeSize :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SchemaResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'databaseInstance', 'schemaResponse_databaseInstance' - The database for a schema in a Fleet Advisor collector inventory.
--
-- 'similarity', 'schemaResponse_similarity' - The similarity value for a schema in a Fleet Advisor collector
-- inventory. A higher similarity value indicates that a schema is likely
-- to be a duplicate.
--
-- 'originalSchema', 'schemaResponse_originalSchema' - Undocumented member.
--
-- 'codeLineCount', 'schemaResponse_codeLineCount' - The number of lines of code in a schema in a Fleet Advisor collector
-- inventory.
--
-- 'schemaName', 'schemaResponse_schemaName' - The name of a schema in a Fleet Advisor collector inventory.
--
-- 'server', 'schemaResponse_server' - The database server for a schema in a Fleet Advisor collector inventory.
--
-- 'complexity', 'schemaResponse_complexity' - The complexity level of the code in a schema in a Fleet Advisor
-- collector inventory.
--
-- 'schemaId', 'schemaResponse_schemaId' - The ID of a schema in a Fleet Advisor collector inventory.
--
-- 'codeSize', 'schemaResponse_codeSize' - The size level of the code in a schema in a Fleet Advisor collector
-- inventory.
newSchemaResponse ::
  SchemaResponse
newSchemaResponse =
  SchemaResponse'
    { databaseInstance = Prelude.Nothing,
      similarity = Prelude.Nothing,
      originalSchema = Prelude.Nothing,
      codeLineCount = Prelude.Nothing,
      schemaName = Prelude.Nothing,
      server = Prelude.Nothing,
      complexity = Prelude.Nothing,
      schemaId = Prelude.Nothing,
      codeSize = Prelude.Nothing
    }

-- | The database for a schema in a Fleet Advisor collector inventory.
schemaResponse_databaseInstance :: Lens.Lens' SchemaResponse (Prelude.Maybe DatabaseShortInfoResponse)
schemaResponse_databaseInstance = Lens.lens (\SchemaResponse' {databaseInstance} -> databaseInstance) (\s@SchemaResponse' {} a -> s {databaseInstance = a} :: SchemaResponse)

-- | The similarity value for a schema in a Fleet Advisor collector
-- inventory. A higher similarity value indicates that a schema is likely
-- to be a duplicate.
schemaResponse_similarity :: Lens.Lens' SchemaResponse (Prelude.Maybe Prelude.Double)
schemaResponse_similarity = Lens.lens (\SchemaResponse' {similarity} -> similarity) (\s@SchemaResponse' {} a -> s {similarity = a} :: SchemaResponse)

-- | Undocumented member.
schemaResponse_originalSchema :: Lens.Lens' SchemaResponse (Prelude.Maybe SchemaShortInfoResponse)
schemaResponse_originalSchema = Lens.lens (\SchemaResponse' {originalSchema} -> originalSchema) (\s@SchemaResponse' {} a -> s {originalSchema = a} :: SchemaResponse)

-- | The number of lines of code in a schema in a Fleet Advisor collector
-- inventory.
schemaResponse_codeLineCount :: Lens.Lens' SchemaResponse (Prelude.Maybe Prelude.Integer)
schemaResponse_codeLineCount = Lens.lens (\SchemaResponse' {codeLineCount} -> codeLineCount) (\s@SchemaResponse' {} a -> s {codeLineCount = a} :: SchemaResponse)

-- | The name of a schema in a Fleet Advisor collector inventory.
schemaResponse_schemaName :: Lens.Lens' SchemaResponse (Prelude.Maybe Prelude.Text)
schemaResponse_schemaName = Lens.lens (\SchemaResponse' {schemaName} -> schemaName) (\s@SchemaResponse' {} a -> s {schemaName = a} :: SchemaResponse)

-- | The database server for a schema in a Fleet Advisor collector inventory.
schemaResponse_server :: Lens.Lens' SchemaResponse (Prelude.Maybe ServerShortInfoResponse)
schemaResponse_server = Lens.lens (\SchemaResponse' {server} -> server) (\s@SchemaResponse' {} a -> s {server = a} :: SchemaResponse)

-- | The complexity level of the code in a schema in a Fleet Advisor
-- collector inventory.
schemaResponse_complexity :: Lens.Lens' SchemaResponse (Prelude.Maybe Prelude.Text)
schemaResponse_complexity = Lens.lens (\SchemaResponse' {complexity} -> complexity) (\s@SchemaResponse' {} a -> s {complexity = a} :: SchemaResponse)

-- | The ID of a schema in a Fleet Advisor collector inventory.
schemaResponse_schemaId :: Lens.Lens' SchemaResponse (Prelude.Maybe Prelude.Text)
schemaResponse_schemaId = Lens.lens (\SchemaResponse' {schemaId} -> schemaId) (\s@SchemaResponse' {} a -> s {schemaId = a} :: SchemaResponse)

-- | The size level of the code in a schema in a Fleet Advisor collector
-- inventory.
schemaResponse_codeSize :: Lens.Lens' SchemaResponse (Prelude.Maybe Prelude.Integer)
schemaResponse_codeSize = Lens.lens (\SchemaResponse' {codeSize} -> codeSize) (\s@SchemaResponse' {} a -> s {codeSize = a} :: SchemaResponse)

instance Core.FromJSON SchemaResponse where
  parseJSON =
    Core.withObject
      "SchemaResponse"
      ( \x ->
          SchemaResponse'
            Prelude.<$> (x Core..:? "DatabaseInstance")
            Prelude.<*> (x Core..:? "Similarity")
            Prelude.<*> (x Core..:? "OriginalSchema")
            Prelude.<*> (x Core..:? "CodeLineCount")
            Prelude.<*> (x Core..:? "SchemaName")
            Prelude.<*> (x Core..:? "Server")
            Prelude.<*> (x Core..:? "Complexity")
            Prelude.<*> (x Core..:? "SchemaId")
            Prelude.<*> (x Core..:? "CodeSize")
      )

instance Prelude.Hashable SchemaResponse where
  hashWithSalt _salt SchemaResponse' {..} =
    _salt `Prelude.hashWithSalt` databaseInstance
      `Prelude.hashWithSalt` similarity
      `Prelude.hashWithSalt` originalSchema
      `Prelude.hashWithSalt` codeLineCount
      `Prelude.hashWithSalt` schemaName
      `Prelude.hashWithSalt` server
      `Prelude.hashWithSalt` complexity
      `Prelude.hashWithSalt` schemaId
      `Prelude.hashWithSalt` codeSize

instance Prelude.NFData SchemaResponse where
  rnf SchemaResponse' {..} =
    Prelude.rnf databaseInstance
      `Prelude.seq` Prelude.rnf similarity
      `Prelude.seq` Prelude.rnf originalSchema
      `Prelude.seq` Prelude.rnf codeLineCount
      `Prelude.seq` Prelude.rnf schemaName
      `Prelude.seq` Prelude.rnf server
      `Prelude.seq` Prelude.rnf complexity
      `Prelude.seq` Prelude.rnf schemaId
      `Prelude.seq` Prelude.rnf codeSize
