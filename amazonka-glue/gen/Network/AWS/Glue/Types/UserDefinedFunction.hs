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
-- Module      : Network.AWS.Glue.Types.UserDefinedFunction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.UserDefinedFunction where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types.PrincipalType
import Network.AWS.Glue.Types.ResourceUri
import qualified Network.AWS.Lens as Lens

-- | Represents the equivalent of a Hive user-defined function (@UDF@)
-- definition.
--
-- /See:/ 'newUserDefinedFunction' smart constructor.
data UserDefinedFunction = UserDefinedFunction'
  { -- | The owner type.
    ownerType :: Core.Maybe PrincipalType,
    -- | The Java class that contains the function code.
    className :: Core.Maybe Core.Text,
    -- | The ID of the Data Catalog in which the function resides.
    catalogId :: Core.Maybe Core.Text,
    -- | The owner of the function.
    ownerName :: Core.Maybe Core.Text,
    -- | The name of the function.
    functionName :: Core.Maybe Core.Text,
    -- | The resource URIs for the function.
    resourceUris :: Core.Maybe [ResourceUri],
    -- | The time at which the function was created.
    createTime :: Core.Maybe Core.POSIX,
    -- | The name of the catalog database that contains the function.
    databaseName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'className', 'userDefinedFunction_className' - The Java class that contains the function code.
--
-- 'catalogId', 'userDefinedFunction_catalogId' - The ID of the Data Catalog in which the function resides.
--
-- 'ownerName', 'userDefinedFunction_ownerName' - The owner of the function.
--
-- 'functionName', 'userDefinedFunction_functionName' - The name of the function.
--
-- 'resourceUris', 'userDefinedFunction_resourceUris' - The resource URIs for the function.
--
-- 'createTime', 'userDefinedFunction_createTime' - The time at which the function was created.
--
-- 'databaseName', 'userDefinedFunction_databaseName' - The name of the catalog database that contains the function.
newUserDefinedFunction ::
  UserDefinedFunction
newUserDefinedFunction =
  UserDefinedFunction'
    { ownerType = Core.Nothing,
      className = Core.Nothing,
      catalogId = Core.Nothing,
      ownerName = Core.Nothing,
      functionName = Core.Nothing,
      resourceUris = Core.Nothing,
      createTime = Core.Nothing,
      databaseName = Core.Nothing
    }

-- | The owner type.
userDefinedFunction_ownerType :: Lens.Lens' UserDefinedFunction (Core.Maybe PrincipalType)
userDefinedFunction_ownerType = Lens.lens (\UserDefinedFunction' {ownerType} -> ownerType) (\s@UserDefinedFunction' {} a -> s {ownerType = a} :: UserDefinedFunction)

-- | The Java class that contains the function code.
userDefinedFunction_className :: Lens.Lens' UserDefinedFunction (Core.Maybe Core.Text)
userDefinedFunction_className = Lens.lens (\UserDefinedFunction' {className} -> className) (\s@UserDefinedFunction' {} a -> s {className = a} :: UserDefinedFunction)

-- | The ID of the Data Catalog in which the function resides.
userDefinedFunction_catalogId :: Lens.Lens' UserDefinedFunction (Core.Maybe Core.Text)
userDefinedFunction_catalogId = Lens.lens (\UserDefinedFunction' {catalogId} -> catalogId) (\s@UserDefinedFunction' {} a -> s {catalogId = a} :: UserDefinedFunction)

-- | The owner of the function.
userDefinedFunction_ownerName :: Lens.Lens' UserDefinedFunction (Core.Maybe Core.Text)
userDefinedFunction_ownerName = Lens.lens (\UserDefinedFunction' {ownerName} -> ownerName) (\s@UserDefinedFunction' {} a -> s {ownerName = a} :: UserDefinedFunction)

-- | The name of the function.
userDefinedFunction_functionName :: Lens.Lens' UserDefinedFunction (Core.Maybe Core.Text)
userDefinedFunction_functionName = Lens.lens (\UserDefinedFunction' {functionName} -> functionName) (\s@UserDefinedFunction' {} a -> s {functionName = a} :: UserDefinedFunction)

-- | The resource URIs for the function.
userDefinedFunction_resourceUris :: Lens.Lens' UserDefinedFunction (Core.Maybe [ResourceUri])
userDefinedFunction_resourceUris = Lens.lens (\UserDefinedFunction' {resourceUris} -> resourceUris) (\s@UserDefinedFunction' {} a -> s {resourceUris = a} :: UserDefinedFunction) Core.. Lens.mapping Lens._Coerce

-- | The time at which the function was created.
userDefinedFunction_createTime :: Lens.Lens' UserDefinedFunction (Core.Maybe Core.UTCTime)
userDefinedFunction_createTime = Lens.lens (\UserDefinedFunction' {createTime} -> createTime) (\s@UserDefinedFunction' {} a -> s {createTime = a} :: UserDefinedFunction) Core.. Lens.mapping Core._Time

-- | The name of the catalog database that contains the function.
userDefinedFunction_databaseName :: Lens.Lens' UserDefinedFunction (Core.Maybe Core.Text)
userDefinedFunction_databaseName = Lens.lens (\UserDefinedFunction' {databaseName} -> databaseName) (\s@UserDefinedFunction' {} a -> s {databaseName = a} :: UserDefinedFunction)

instance Core.FromJSON UserDefinedFunction where
  parseJSON =
    Core.withObject
      "UserDefinedFunction"
      ( \x ->
          UserDefinedFunction'
            Core.<$> (x Core..:? "OwnerType")
            Core.<*> (x Core..:? "ClassName")
            Core.<*> (x Core..:? "CatalogId")
            Core.<*> (x Core..:? "OwnerName")
            Core.<*> (x Core..:? "FunctionName")
            Core.<*> (x Core..:? "ResourceUris" Core..!= Core.mempty)
            Core.<*> (x Core..:? "CreateTime")
            Core.<*> (x Core..:? "DatabaseName")
      )

instance Core.Hashable UserDefinedFunction

instance Core.NFData UserDefinedFunction
