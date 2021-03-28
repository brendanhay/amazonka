{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.FunctionConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppSync.Types.FunctionConfiguration
  ( FunctionConfiguration (..)
  -- * Smart constructor
  , mkFunctionConfiguration
  -- * Lenses
  , fcDataSourceName
  , fcDescription
  , fcFunctionArn
  , fcFunctionId
  , fcFunctionVersion
  , fcName
  , fcRequestMappingTemplate
  , fcResponseMappingTemplate
  ) where

import qualified Network.AWS.AppSync.Types.MappingTemplate as Types
import qualified Network.AWS.AppSync.Types.ResourceName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A function is a reusable entity. Multiple functions can be used to compose the resolver logic.
--
-- /See:/ 'mkFunctionConfiguration' smart constructor.
data FunctionConfiguration = FunctionConfiguration'
  { dataSourceName :: Core.Maybe Types.ResourceName
    -- ^ The name of the @DataSource@ .
  , description :: Core.Maybe Core.Text
    -- ^ The @Function@ description.
  , functionArn :: Core.Maybe Core.Text
    -- ^ The ARN of the @Function@ object.
  , functionId :: Core.Maybe Core.Text
    -- ^ A unique ID representing the @Function@ object.
  , functionVersion :: Core.Maybe Core.Text
    -- ^ The version of the request mapping template. Currently only the 2018-05-29 version of the template is supported.
  , name :: Core.Maybe Types.ResourceName
    -- ^ The name of the @Function@ object.
  , requestMappingTemplate :: Core.Maybe Types.MappingTemplate
    -- ^ The @Function@ request mapping template. Functions support only the 2018-05-29 version of the request mapping template.
  , responseMappingTemplate :: Core.Maybe Types.MappingTemplate
    -- ^ The @Function@ response mapping template.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FunctionConfiguration' value with any optional fields omitted.
mkFunctionConfiguration
    :: FunctionConfiguration
mkFunctionConfiguration
  = FunctionConfiguration'{dataSourceName = Core.Nothing,
                           description = Core.Nothing, functionArn = Core.Nothing,
                           functionId = Core.Nothing, functionVersion = Core.Nothing,
                           name = Core.Nothing, requestMappingTemplate = Core.Nothing,
                           responseMappingTemplate = Core.Nothing}

-- | The name of the @DataSource@ .
--
-- /Note:/ Consider using 'dataSourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcDataSourceName :: Lens.Lens' FunctionConfiguration (Core.Maybe Types.ResourceName)
fcDataSourceName = Lens.field @"dataSourceName"
{-# INLINEABLE fcDataSourceName #-}
{-# DEPRECATED dataSourceName "Use generic-lens or generic-optics with 'dataSourceName' instead"  #-}

-- | The @Function@ description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcDescription :: Lens.Lens' FunctionConfiguration (Core.Maybe Core.Text)
fcDescription = Lens.field @"description"
{-# INLINEABLE fcDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The ARN of the @Function@ object.
--
-- /Note:/ Consider using 'functionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcFunctionArn :: Lens.Lens' FunctionConfiguration (Core.Maybe Core.Text)
fcFunctionArn = Lens.field @"functionArn"
{-# INLINEABLE fcFunctionArn #-}
{-# DEPRECATED functionArn "Use generic-lens or generic-optics with 'functionArn' instead"  #-}

-- | A unique ID representing the @Function@ object.
--
-- /Note:/ Consider using 'functionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcFunctionId :: Lens.Lens' FunctionConfiguration (Core.Maybe Core.Text)
fcFunctionId = Lens.field @"functionId"
{-# INLINEABLE fcFunctionId #-}
{-# DEPRECATED functionId "Use generic-lens or generic-optics with 'functionId' instead"  #-}

-- | The version of the request mapping template. Currently only the 2018-05-29 version of the template is supported.
--
-- /Note:/ Consider using 'functionVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcFunctionVersion :: Lens.Lens' FunctionConfiguration (Core.Maybe Core.Text)
fcFunctionVersion = Lens.field @"functionVersion"
{-# INLINEABLE fcFunctionVersion #-}
{-# DEPRECATED functionVersion "Use generic-lens or generic-optics with 'functionVersion' instead"  #-}

-- | The name of the @Function@ object.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcName :: Lens.Lens' FunctionConfiguration (Core.Maybe Types.ResourceName)
fcName = Lens.field @"name"
{-# INLINEABLE fcName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The @Function@ request mapping template. Functions support only the 2018-05-29 version of the request mapping template.
--
-- /Note:/ Consider using 'requestMappingTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcRequestMappingTemplate :: Lens.Lens' FunctionConfiguration (Core.Maybe Types.MappingTemplate)
fcRequestMappingTemplate = Lens.field @"requestMappingTemplate"
{-# INLINEABLE fcRequestMappingTemplate #-}
{-# DEPRECATED requestMappingTemplate "Use generic-lens or generic-optics with 'requestMappingTemplate' instead"  #-}

-- | The @Function@ response mapping template.
--
-- /Note:/ Consider using 'responseMappingTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcResponseMappingTemplate :: Lens.Lens' FunctionConfiguration (Core.Maybe Types.MappingTemplate)
fcResponseMappingTemplate = Lens.field @"responseMappingTemplate"
{-# INLINEABLE fcResponseMappingTemplate #-}
{-# DEPRECATED responseMappingTemplate "Use generic-lens or generic-optics with 'responseMappingTemplate' instead"  #-}

instance Core.FromJSON FunctionConfiguration where
        parseJSON
          = Core.withObject "FunctionConfiguration" Core.$
              \ x ->
                FunctionConfiguration' Core.<$>
                  (x Core..:? "dataSourceName") Core.<*> x Core..:? "description"
                    Core.<*> x Core..:? "functionArn"
                    Core.<*> x Core..:? "functionId"
                    Core.<*> x Core..:? "functionVersion"
                    Core.<*> x Core..:? "name"
                    Core.<*> x Core..:? "requestMappingTemplate"
                    Core.<*> x Core..:? "responseMappingTemplate"
