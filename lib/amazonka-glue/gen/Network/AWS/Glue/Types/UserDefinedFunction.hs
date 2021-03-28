{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.UserDefinedFunction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.UserDefinedFunction
  ( UserDefinedFunction (..)
  -- * Smart constructor
  , mkUserDefinedFunction
  -- * Lenses
  , udfCatalogId
  , udfClassName
  , udfCreateTime
  , udfDatabaseName
  , udfFunctionName
  , udfOwnerName
  , udfOwnerType
  , udfResourceUris
  ) where

import qualified Network.AWS.Glue.Types.CatalogId as Types
import qualified Network.AWS.Glue.Types.ClassName as Types
import qualified Network.AWS.Glue.Types.DatabaseName as Types
import qualified Network.AWS.Glue.Types.FunctionName as Types
import qualified Network.AWS.Glue.Types.OwnerName as Types
import qualified Network.AWS.Glue.Types.PrincipalType as Types
import qualified Network.AWS.Glue.Types.ResourceUri as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the equivalent of a Hive user-defined function (@UDF@ ) definition.
--
-- /See:/ 'mkUserDefinedFunction' smart constructor.
data UserDefinedFunction = UserDefinedFunction'
  { catalogId :: Core.Maybe Types.CatalogId
    -- ^ The ID of the Data Catalog in which the function resides.
  , className :: Core.Maybe Types.ClassName
    -- ^ The Java class that contains the function code.
  , createTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time at which the function was created.
  , databaseName :: Core.Maybe Types.DatabaseName
    -- ^ The name of the catalog database that contains the function.
  , functionName :: Core.Maybe Types.FunctionName
    -- ^ The name of the function.
  , ownerName :: Core.Maybe Types.OwnerName
    -- ^ The owner of the function.
  , ownerType :: Core.Maybe Types.PrincipalType
    -- ^ The owner type.
  , resourceUris :: Core.Maybe [Types.ResourceUri]
    -- ^ The resource URIs for the function.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UserDefinedFunction' value with any optional fields omitted.
mkUserDefinedFunction
    :: UserDefinedFunction
mkUserDefinedFunction
  = UserDefinedFunction'{catalogId = Core.Nothing,
                         className = Core.Nothing, createTime = Core.Nothing,
                         databaseName = Core.Nothing, functionName = Core.Nothing,
                         ownerName = Core.Nothing, ownerType = Core.Nothing,
                         resourceUris = Core.Nothing}

-- | The ID of the Data Catalog in which the function resides.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udfCatalogId :: Lens.Lens' UserDefinedFunction (Core.Maybe Types.CatalogId)
udfCatalogId = Lens.field @"catalogId"
{-# INLINEABLE udfCatalogId #-}
{-# DEPRECATED catalogId "Use generic-lens or generic-optics with 'catalogId' instead"  #-}

-- | The Java class that contains the function code.
--
-- /Note:/ Consider using 'className' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udfClassName :: Lens.Lens' UserDefinedFunction (Core.Maybe Types.ClassName)
udfClassName = Lens.field @"className"
{-# INLINEABLE udfClassName #-}
{-# DEPRECATED className "Use generic-lens or generic-optics with 'className' instead"  #-}

-- | The time at which the function was created.
--
-- /Note:/ Consider using 'createTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udfCreateTime :: Lens.Lens' UserDefinedFunction (Core.Maybe Core.NominalDiffTime)
udfCreateTime = Lens.field @"createTime"
{-# INLINEABLE udfCreateTime #-}
{-# DEPRECATED createTime "Use generic-lens or generic-optics with 'createTime' instead"  #-}

-- | The name of the catalog database that contains the function.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udfDatabaseName :: Lens.Lens' UserDefinedFunction (Core.Maybe Types.DatabaseName)
udfDatabaseName = Lens.field @"databaseName"
{-# INLINEABLE udfDatabaseName #-}
{-# DEPRECATED databaseName "Use generic-lens or generic-optics with 'databaseName' instead"  #-}

-- | The name of the function.
--
-- /Note:/ Consider using 'functionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udfFunctionName :: Lens.Lens' UserDefinedFunction (Core.Maybe Types.FunctionName)
udfFunctionName = Lens.field @"functionName"
{-# INLINEABLE udfFunctionName #-}
{-# DEPRECATED functionName "Use generic-lens or generic-optics with 'functionName' instead"  #-}

-- | The owner of the function.
--
-- /Note:/ Consider using 'ownerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udfOwnerName :: Lens.Lens' UserDefinedFunction (Core.Maybe Types.OwnerName)
udfOwnerName = Lens.field @"ownerName"
{-# INLINEABLE udfOwnerName #-}
{-# DEPRECATED ownerName "Use generic-lens or generic-optics with 'ownerName' instead"  #-}

-- | The owner type.
--
-- /Note:/ Consider using 'ownerType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udfOwnerType :: Lens.Lens' UserDefinedFunction (Core.Maybe Types.PrincipalType)
udfOwnerType = Lens.field @"ownerType"
{-# INLINEABLE udfOwnerType #-}
{-# DEPRECATED ownerType "Use generic-lens or generic-optics with 'ownerType' instead"  #-}

-- | The resource URIs for the function.
--
-- /Note:/ Consider using 'resourceUris' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udfResourceUris :: Lens.Lens' UserDefinedFunction (Core.Maybe [Types.ResourceUri])
udfResourceUris = Lens.field @"resourceUris"
{-# INLINEABLE udfResourceUris #-}
{-# DEPRECATED resourceUris "Use generic-lens or generic-optics with 'resourceUris' instead"  #-}

instance Core.FromJSON UserDefinedFunction where
        parseJSON
          = Core.withObject "UserDefinedFunction" Core.$
              \ x ->
                UserDefinedFunction' Core.<$>
                  (x Core..:? "CatalogId") Core.<*> x Core..:? "ClassName" Core.<*>
                    x Core..:? "CreateTime"
                    Core.<*> x Core..:? "DatabaseName"
                    Core.<*> x Core..:? "FunctionName"
                    Core.<*> x Core..:? "OwnerName"
                    Core.<*> x Core..:? "OwnerType"
                    Core.<*> x Core..:? "ResourceUris"
