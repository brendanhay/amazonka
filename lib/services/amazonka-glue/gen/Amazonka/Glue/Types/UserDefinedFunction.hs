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
-- Module      : Amazonka.Glue.Types.UserDefinedFunction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.UserDefinedFunction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Glue.Types.PrincipalType
import Amazonka.Glue.Types.ResourceUri
import qualified Amazonka.Prelude as Prelude

-- | Represents the equivalent of a Hive user-defined function (@UDF@)
-- definition.
--
-- /See:/ 'newUserDefinedFunction' smart constructor.
data UserDefinedFunction = UserDefinedFunction'
  { -- | The owner type.
    ownerType :: Prelude.Maybe PrincipalType,
    -- | The owner of the function.
    ownerName :: Prelude.Maybe Prelude.Text,
    -- | The name of the catalog database that contains the function.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | The resource URIs for the function.
    resourceUris :: Prelude.Maybe [ResourceUri],
    -- | The name of the function.
    functionName :: Prelude.Maybe Prelude.Text,
    -- | The Java class that contains the function code.
    className :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Data Catalog in which the function resides.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The time at which the function was created.
    createTime :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserDefinedFunction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ownerType', 'userDefinedFunction_ownerType' - The owner type.
--
-- 'ownerName', 'userDefinedFunction_ownerName' - The owner of the function.
--
-- 'databaseName', 'userDefinedFunction_databaseName' - The name of the catalog database that contains the function.
--
-- 'resourceUris', 'userDefinedFunction_resourceUris' - The resource URIs for the function.
--
-- 'functionName', 'userDefinedFunction_functionName' - The name of the function.
--
-- 'className', 'userDefinedFunction_className' - The Java class that contains the function code.
--
-- 'catalogId', 'userDefinedFunction_catalogId' - The ID of the Data Catalog in which the function resides.
--
-- 'createTime', 'userDefinedFunction_createTime' - The time at which the function was created.
newUserDefinedFunction ::
  UserDefinedFunction
newUserDefinedFunction =
  UserDefinedFunction'
    { ownerType = Prelude.Nothing,
      ownerName = Prelude.Nothing,
      databaseName = Prelude.Nothing,
      resourceUris = Prelude.Nothing,
      functionName = Prelude.Nothing,
      className = Prelude.Nothing,
      catalogId = Prelude.Nothing,
      createTime = Prelude.Nothing
    }

-- | The owner type.
userDefinedFunction_ownerType :: Lens.Lens' UserDefinedFunction (Prelude.Maybe PrincipalType)
userDefinedFunction_ownerType = Lens.lens (\UserDefinedFunction' {ownerType} -> ownerType) (\s@UserDefinedFunction' {} a -> s {ownerType = a} :: UserDefinedFunction)

-- | The owner of the function.
userDefinedFunction_ownerName :: Lens.Lens' UserDefinedFunction (Prelude.Maybe Prelude.Text)
userDefinedFunction_ownerName = Lens.lens (\UserDefinedFunction' {ownerName} -> ownerName) (\s@UserDefinedFunction' {} a -> s {ownerName = a} :: UserDefinedFunction)

-- | The name of the catalog database that contains the function.
userDefinedFunction_databaseName :: Lens.Lens' UserDefinedFunction (Prelude.Maybe Prelude.Text)
userDefinedFunction_databaseName = Lens.lens (\UserDefinedFunction' {databaseName} -> databaseName) (\s@UserDefinedFunction' {} a -> s {databaseName = a} :: UserDefinedFunction)

-- | The resource URIs for the function.
userDefinedFunction_resourceUris :: Lens.Lens' UserDefinedFunction (Prelude.Maybe [ResourceUri])
userDefinedFunction_resourceUris = Lens.lens (\UserDefinedFunction' {resourceUris} -> resourceUris) (\s@UserDefinedFunction' {} a -> s {resourceUris = a} :: UserDefinedFunction) Prelude.. Lens.mapping Lens.coerced

-- | The name of the function.
userDefinedFunction_functionName :: Lens.Lens' UserDefinedFunction (Prelude.Maybe Prelude.Text)
userDefinedFunction_functionName = Lens.lens (\UserDefinedFunction' {functionName} -> functionName) (\s@UserDefinedFunction' {} a -> s {functionName = a} :: UserDefinedFunction)

-- | The Java class that contains the function code.
userDefinedFunction_className :: Lens.Lens' UserDefinedFunction (Prelude.Maybe Prelude.Text)
userDefinedFunction_className = Lens.lens (\UserDefinedFunction' {className} -> className) (\s@UserDefinedFunction' {} a -> s {className = a} :: UserDefinedFunction)

-- | The ID of the Data Catalog in which the function resides.
userDefinedFunction_catalogId :: Lens.Lens' UserDefinedFunction (Prelude.Maybe Prelude.Text)
userDefinedFunction_catalogId = Lens.lens (\UserDefinedFunction' {catalogId} -> catalogId) (\s@UserDefinedFunction' {} a -> s {catalogId = a} :: UserDefinedFunction)

-- | The time at which the function was created.
userDefinedFunction_createTime :: Lens.Lens' UserDefinedFunction (Prelude.Maybe Prelude.UTCTime)
userDefinedFunction_createTime = Lens.lens (\UserDefinedFunction' {createTime} -> createTime) (\s@UserDefinedFunction' {} a -> s {createTime = a} :: UserDefinedFunction) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON UserDefinedFunction where
  parseJSON =
    Core.withObject
      "UserDefinedFunction"
      ( \x ->
          UserDefinedFunction'
            Prelude.<$> (x Core..:? "OwnerType")
            Prelude.<*> (x Core..:? "OwnerName")
            Prelude.<*> (x Core..:? "DatabaseName")
            Prelude.<*> (x Core..:? "ResourceUris" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "FunctionName")
            Prelude.<*> (x Core..:? "ClassName")
            Prelude.<*> (x Core..:? "CatalogId")
            Prelude.<*> (x Core..:? "CreateTime")
      )

instance Prelude.Hashable UserDefinedFunction where
  hashWithSalt _salt UserDefinedFunction' {..} =
    _salt `Prelude.hashWithSalt` ownerType
      `Prelude.hashWithSalt` ownerName
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` resourceUris
      `Prelude.hashWithSalt` functionName
      `Prelude.hashWithSalt` className
      `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` createTime

instance Prelude.NFData UserDefinedFunction where
  rnf UserDefinedFunction' {..} =
    Prelude.rnf ownerType
      `Prelude.seq` Prelude.rnf ownerName
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf resourceUris
      `Prelude.seq` Prelude.rnf functionName
      `Prelude.seq` Prelude.rnf className
      `Prelude.seq` Prelude.rnf catalogId
      `Prelude.seq` Prelude.rnf createTime
