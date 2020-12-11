-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.UserDefinedFunction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.UserDefinedFunction
  ( UserDefinedFunction (..),

    -- * Smart constructor
    mkUserDefinedFunction,

    -- * Lenses
    udfOwnerName,
    udfCatalogId,
    udfResourceURIs,
    udfDatabaseName,
    udfFunctionName,
    udfOwnerType,
    udfCreateTime,
    udfClassName,
  )
where

import Network.AWS.Glue.Types.PrincipalType
import Network.AWS.Glue.Types.ResourceURI
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the equivalent of a Hive user-defined function (@UDF@ ) definition.
--
-- /See:/ 'mkUserDefinedFunction' smart constructor.
data UserDefinedFunction = UserDefinedFunction'
  { ownerName ::
      Lude.Maybe Lude.Text,
    catalogId :: Lude.Maybe Lude.Text,
    resourceURIs :: Lude.Maybe [ResourceURI],
    databaseName :: Lude.Maybe Lude.Text,
    functionName :: Lude.Maybe Lude.Text,
    ownerType :: Lude.Maybe PrincipalType,
    createTime :: Lude.Maybe Lude.Timestamp,
    className :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UserDefinedFunction' with the minimum fields required to make a request.
--
-- * 'catalogId' - The ID of the Data Catalog in which the function resides.
-- * 'className' - The Java class that contains the function code.
-- * 'createTime' - The time at which the function was created.
-- * 'databaseName' - The name of the catalog database that contains the function.
-- * 'functionName' - The name of the function.
-- * 'ownerName' - The owner of the function.
-- * 'ownerType' - The owner type.
-- * 'resourceURIs' - The resource URIs for the function.
mkUserDefinedFunction ::
  UserDefinedFunction
mkUserDefinedFunction =
  UserDefinedFunction'
    { ownerName = Lude.Nothing,
      catalogId = Lude.Nothing,
      resourceURIs = Lude.Nothing,
      databaseName = Lude.Nothing,
      functionName = Lude.Nothing,
      ownerType = Lude.Nothing,
      createTime = Lude.Nothing,
      className = Lude.Nothing
    }

-- | The owner of the function.
--
-- /Note:/ Consider using 'ownerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udfOwnerName :: Lens.Lens' UserDefinedFunction (Lude.Maybe Lude.Text)
udfOwnerName = Lens.lens (ownerName :: UserDefinedFunction -> Lude.Maybe Lude.Text) (\s a -> s {ownerName = a} :: UserDefinedFunction)
{-# DEPRECATED udfOwnerName "Use generic-lens or generic-optics with 'ownerName' instead." #-}

-- | The ID of the Data Catalog in which the function resides.
--
-- /Note:/ Consider using 'catalogId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udfCatalogId :: Lens.Lens' UserDefinedFunction (Lude.Maybe Lude.Text)
udfCatalogId = Lens.lens (catalogId :: UserDefinedFunction -> Lude.Maybe Lude.Text) (\s a -> s {catalogId = a} :: UserDefinedFunction)
{-# DEPRECATED udfCatalogId "Use generic-lens or generic-optics with 'catalogId' instead." #-}

-- | The resource URIs for the function.
--
-- /Note:/ Consider using 'resourceURIs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udfResourceURIs :: Lens.Lens' UserDefinedFunction (Lude.Maybe [ResourceURI])
udfResourceURIs = Lens.lens (resourceURIs :: UserDefinedFunction -> Lude.Maybe [ResourceURI]) (\s a -> s {resourceURIs = a} :: UserDefinedFunction)
{-# DEPRECATED udfResourceURIs "Use generic-lens or generic-optics with 'resourceURIs' instead." #-}

-- | The name of the catalog database that contains the function.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udfDatabaseName :: Lens.Lens' UserDefinedFunction (Lude.Maybe Lude.Text)
udfDatabaseName = Lens.lens (databaseName :: UserDefinedFunction -> Lude.Maybe Lude.Text) (\s a -> s {databaseName = a} :: UserDefinedFunction)
{-# DEPRECATED udfDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The name of the function.
--
-- /Note:/ Consider using 'functionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udfFunctionName :: Lens.Lens' UserDefinedFunction (Lude.Maybe Lude.Text)
udfFunctionName = Lens.lens (functionName :: UserDefinedFunction -> Lude.Maybe Lude.Text) (\s a -> s {functionName = a} :: UserDefinedFunction)
{-# DEPRECATED udfFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

-- | The owner type.
--
-- /Note:/ Consider using 'ownerType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udfOwnerType :: Lens.Lens' UserDefinedFunction (Lude.Maybe PrincipalType)
udfOwnerType = Lens.lens (ownerType :: UserDefinedFunction -> Lude.Maybe PrincipalType) (\s a -> s {ownerType = a} :: UserDefinedFunction)
{-# DEPRECATED udfOwnerType "Use generic-lens or generic-optics with 'ownerType' instead." #-}

-- | The time at which the function was created.
--
-- /Note:/ Consider using 'createTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udfCreateTime :: Lens.Lens' UserDefinedFunction (Lude.Maybe Lude.Timestamp)
udfCreateTime = Lens.lens (createTime :: UserDefinedFunction -> Lude.Maybe Lude.Timestamp) (\s a -> s {createTime = a} :: UserDefinedFunction)
{-# DEPRECATED udfCreateTime "Use generic-lens or generic-optics with 'createTime' instead." #-}

-- | The Java class that contains the function code.
--
-- /Note:/ Consider using 'className' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udfClassName :: Lens.Lens' UserDefinedFunction (Lude.Maybe Lude.Text)
udfClassName = Lens.lens (className :: UserDefinedFunction -> Lude.Maybe Lude.Text) (\s a -> s {className = a} :: UserDefinedFunction)
{-# DEPRECATED udfClassName "Use generic-lens or generic-optics with 'className' instead." #-}

instance Lude.FromJSON UserDefinedFunction where
  parseJSON =
    Lude.withObject
      "UserDefinedFunction"
      ( \x ->
          UserDefinedFunction'
            Lude.<$> (x Lude..:? "OwnerName")
            Lude.<*> (x Lude..:? "CatalogId")
            Lude.<*> (x Lude..:? "ResourceUris" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "DatabaseName")
            Lude.<*> (x Lude..:? "FunctionName")
            Lude.<*> (x Lude..:? "OwnerType")
            Lude.<*> (x Lude..:? "CreateTime")
            Lude.<*> (x Lude..:? "ClassName")
      )
