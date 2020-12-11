-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.Resolver
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.Resolver
  ( Resolver (..),

    -- * Smart constructor
    mkResolver,

    -- * Lenses
    rTypeName,
    rDataSourceName,
    rRequestMappingTemplate,
    rKind,
    rResolverARN,
    rCachingConfig,
    rResponseMappingTemplate,
    rFieldName,
    rSyncConfig,
    rPipelineConfig,
  )
where

import Network.AWS.AppSync.Types.CachingConfig
import Network.AWS.AppSync.Types.PipelineConfig
import Network.AWS.AppSync.Types.ResolverKind
import Network.AWS.AppSync.Types.SyncConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a resolver.
--
-- /See:/ 'mkResolver' smart constructor.
data Resolver = Resolver'
  { typeName :: Lude.Maybe Lude.Text,
    dataSourceName :: Lude.Maybe Lude.Text,
    requestMappingTemplate :: Lude.Maybe Lude.Text,
    kind :: Lude.Maybe ResolverKind,
    resolverARN :: Lude.Maybe Lude.Text,
    cachingConfig :: Lude.Maybe CachingConfig,
    responseMappingTemplate :: Lude.Maybe Lude.Text,
    fieldName :: Lude.Maybe Lude.Text,
    syncConfig :: Lude.Maybe SyncConfig,
    pipelineConfig :: Lude.Maybe PipelineConfig
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Resolver' with the minimum fields required to make a request.
--
-- * 'cachingConfig' - The caching configuration for the resolver.
-- * 'dataSourceName' - The resolver data source name.
-- * 'fieldName' - The resolver field name.
-- * 'kind' - The resolver type.
--
--
--     * __UNIT__ : A UNIT resolver type. A UNIT resolver is the default resolver type. A UNIT resolver enables you to execute a GraphQL query against a single data source.
--
--
--     * __PIPELINE__ : A PIPELINE resolver type. A PIPELINE resolver enables you to execute a series of @Function@ in a serial manner. You can use a pipeline resolver to execute a GraphQL query against multiple data sources.
--
--
-- * 'pipelineConfig' - The @PipelineConfig@ .
-- * 'requestMappingTemplate' - The request mapping template.
-- * 'resolverARN' - The resolver ARN.
-- * 'responseMappingTemplate' - The response mapping template.
-- * 'syncConfig' - The @SyncConfig@ for a resolver attached to a versioned datasource.
-- * 'typeName' - The resolver type name.
mkResolver ::
  Resolver
mkResolver =
  Resolver'
    { typeName = Lude.Nothing,
      dataSourceName = Lude.Nothing,
      requestMappingTemplate = Lude.Nothing,
      kind = Lude.Nothing,
      resolverARN = Lude.Nothing,
      cachingConfig = Lude.Nothing,
      responseMappingTemplate = Lude.Nothing,
      fieldName = Lude.Nothing,
      syncConfig = Lude.Nothing,
      pipelineConfig = Lude.Nothing
    }

-- | The resolver type name.
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rTypeName :: Lens.Lens' Resolver (Lude.Maybe Lude.Text)
rTypeName = Lens.lens (typeName :: Resolver -> Lude.Maybe Lude.Text) (\s a -> s {typeName = a} :: Resolver)
{-# DEPRECATED rTypeName "Use generic-lens or generic-optics with 'typeName' instead." #-}

-- | The resolver data source name.
--
-- /Note:/ Consider using 'dataSourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDataSourceName :: Lens.Lens' Resolver (Lude.Maybe Lude.Text)
rDataSourceName = Lens.lens (dataSourceName :: Resolver -> Lude.Maybe Lude.Text) (\s a -> s {dataSourceName = a} :: Resolver)
{-# DEPRECATED rDataSourceName "Use generic-lens or generic-optics with 'dataSourceName' instead." #-}

-- | The request mapping template.
--
-- /Note:/ Consider using 'requestMappingTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rRequestMappingTemplate :: Lens.Lens' Resolver (Lude.Maybe Lude.Text)
rRequestMappingTemplate = Lens.lens (requestMappingTemplate :: Resolver -> Lude.Maybe Lude.Text) (\s a -> s {requestMappingTemplate = a} :: Resolver)
{-# DEPRECATED rRequestMappingTemplate "Use generic-lens or generic-optics with 'requestMappingTemplate' instead." #-}

-- | The resolver type.
--
--
--     * __UNIT__ : A UNIT resolver type. A UNIT resolver is the default resolver type. A UNIT resolver enables you to execute a GraphQL query against a single data source.
--
--
--     * __PIPELINE__ : A PIPELINE resolver type. A PIPELINE resolver enables you to execute a series of @Function@ in a serial manner. You can use a pipeline resolver to execute a GraphQL query against multiple data sources.
--
--
--
-- /Note:/ Consider using 'kind' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rKind :: Lens.Lens' Resolver (Lude.Maybe ResolverKind)
rKind = Lens.lens (kind :: Resolver -> Lude.Maybe ResolverKind) (\s a -> s {kind = a} :: Resolver)
{-# DEPRECATED rKind "Use generic-lens or generic-optics with 'kind' instead." #-}

-- | The resolver ARN.
--
-- /Note:/ Consider using 'resolverARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rResolverARN :: Lens.Lens' Resolver (Lude.Maybe Lude.Text)
rResolverARN = Lens.lens (resolverARN :: Resolver -> Lude.Maybe Lude.Text) (\s a -> s {resolverARN = a} :: Resolver)
{-# DEPRECATED rResolverARN "Use generic-lens or generic-optics with 'resolverARN' instead." #-}

-- | The caching configuration for the resolver.
--
-- /Note:/ Consider using 'cachingConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rCachingConfig :: Lens.Lens' Resolver (Lude.Maybe CachingConfig)
rCachingConfig = Lens.lens (cachingConfig :: Resolver -> Lude.Maybe CachingConfig) (\s a -> s {cachingConfig = a} :: Resolver)
{-# DEPRECATED rCachingConfig "Use generic-lens or generic-optics with 'cachingConfig' instead." #-}

-- | The response mapping template.
--
-- /Note:/ Consider using 'responseMappingTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rResponseMappingTemplate :: Lens.Lens' Resolver (Lude.Maybe Lude.Text)
rResponseMappingTemplate = Lens.lens (responseMappingTemplate :: Resolver -> Lude.Maybe Lude.Text) (\s a -> s {responseMappingTemplate = a} :: Resolver)
{-# DEPRECATED rResponseMappingTemplate "Use generic-lens or generic-optics with 'responseMappingTemplate' instead." #-}

-- | The resolver field name.
--
-- /Note:/ Consider using 'fieldName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rFieldName :: Lens.Lens' Resolver (Lude.Maybe Lude.Text)
rFieldName = Lens.lens (fieldName :: Resolver -> Lude.Maybe Lude.Text) (\s a -> s {fieldName = a} :: Resolver)
{-# DEPRECATED rFieldName "Use generic-lens or generic-optics with 'fieldName' instead." #-}

-- | The @SyncConfig@ for a resolver attached to a versioned datasource.
--
-- /Note:/ Consider using 'syncConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rSyncConfig :: Lens.Lens' Resolver (Lude.Maybe SyncConfig)
rSyncConfig = Lens.lens (syncConfig :: Resolver -> Lude.Maybe SyncConfig) (\s a -> s {syncConfig = a} :: Resolver)
{-# DEPRECATED rSyncConfig "Use generic-lens or generic-optics with 'syncConfig' instead." #-}

-- | The @PipelineConfig@ .
--
-- /Note:/ Consider using 'pipelineConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rPipelineConfig :: Lens.Lens' Resolver (Lude.Maybe PipelineConfig)
rPipelineConfig = Lens.lens (pipelineConfig :: Resolver -> Lude.Maybe PipelineConfig) (\s a -> s {pipelineConfig = a} :: Resolver)
{-# DEPRECATED rPipelineConfig "Use generic-lens or generic-optics with 'pipelineConfig' instead." #-}

instance Lude.FromJSON Resolver where
  parseJSON =
    Lude.withObject
      "Resolver"
      ( \x ->
          Resolver'
            Lude.<$> (x Lude..:? "typeName")
            Lude.<*> (x Lude..:? "dataSourceName")
            Lude.<*> (x Lude..:? "requestMappingTemplate")
            Lude.<*> (x Lude..:? "kind")
            Lude.<*> (x Lude..:? "resolverArn")
            Lude.<*> (x Lude..:? "cachingConfig")
            Lude.<*> (x Lude..:? "responseMappingTemplate")
            Lude.<*> (x Lude..:? "fieldName")
            Lude.<*> (x Lude..:? "syncConfig")
            Lude.<*> (x Lude..:? "pipelineConfig")
      )
