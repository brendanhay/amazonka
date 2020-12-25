{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.FunctionConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.FunctionConfiguration
  ( FunctionConfiguration (..),

    -- * Smart constructor
    mkFunctionConfiguration,

    -- * Lenses
    fcDataSourceName,
    fcDescription,
    fcFunctionArn,
    fcFunctionId,
    fcFunctionVersion,
    fcName,
    fcRequestMappingTemplate,
    fcResponseMappingTemplate,
  )
where

import qualified Network.AWS.AppSync.Types.MappingTemplate as Types
import qualified Network.AWS.AppSync.Types.ResourceName as Types
import qualified Network.AWS.AppSync.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A function is a reusable entity. Multiple functions can be used to compose the resolver logic.
--
-- /See:/ 'mkFunctionConfiguration' smart constructor.
data FunctionConfiguration = FunctionConfiguration'
  { -- | The name of the @DataSource@ .
    dataSourceName :: Core.Maybe Types.ResourceName,
    -- | The @Function@ description.
    description :: Core.Maybe Types.String,
    -- | The ARN of the @Function@ object.
    functionArn :: Core.Maybe Types.String,
    -- | A unique ID representing the @Function@ object.
    functionId :: Core.Maybe Types.String,
    -- | The version of the request mapping template. Currently only the 2018-05-29 version of the template is supported.
    functionVersion :: Core.Maybe Types.String,
    -- | The name of the @Function@ object.
    name :: Core.Maybe Types.ResourceName,
    -- | The @Function@ request mapping template. Functions support only the 2018-05-29 version of the request mapping template.
    requestMappingTemplate :: Core.Maybe Types.MappingTemplate,
    -- | The @Function@ response mapping template.
    responseMappingTemplate :: Core.Maybe Types.MappingTemplate
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FunctionConfiguration' value with any optional fields omitted.
mkFunctionConfiguration ::
  FunctionConfiguration
mkFunctionConfiguration =
  FunctionConfiguration'
    { dataSourceName = Core.Nothing,
      description = Core.Nothing,
      functionArn = Core.Nothing,
      functionId = Core.Nothing,
      functionVersion = Core.Nothing,
      name = Core.Nothing,
      requestMappingTemplate = Core.Nothing,
      responseMappingTemplate = Core.Nothing
    }

-- | The name of the @DataSource@ .
--
-- /Note:/ Consider using 'dataSourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcDataSourceName :: Lens.Lens' FunctionConfiguration (Core.Maybe Types.ResourceName)
fcDataSourceName = Lens.field @"dataSourceName"
{-# DEPRECATED fcDataSourceName "Use generic-lens or generic-optics with 'dataSourceName' instead." #-}

-- | The @Function@ description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcDescription :: Lens.Lens' FunctionConfiguration (Core.Maybe Types.String)
fcDescription = Lens.field @"description"
{-# DEPRECATED fcDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The ARN of the @Function@ object.
--
-- /Note:/ Consider using 'functionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcFunctionArn :: Lens.Lens' FunctionConfiguration (Core.Maybe Types.String)
fcFunctionArn = Lens.field @"functionArn"
{-# DEPRECATED fcFunctionArn "Use generic-lens or generic-optics with 'functionArn' instead." #-}

-- | A unique ID representing the @Function@ object.
--
-- /Note:/ Consider using 'functionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcFunctionId :: Lens.Lens' FunctionConfiguration (Core.Maybe Types.String)
fcFunctionId = Lens.field @"functionId"
{-# DEPRECATED fcFunctionId "Use generic-lens or generic-optics with 'functionId' instead." #-}

-- | The version of the request mapping template. Currently only the 2018-05-29 version of the template is supported.
--
-- /Note:/ Consider using 'functionVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcFunctionVersion :: Lens.Lens' FunctionConfiguration (Core.Maybe Types.String)
fcFunctionVersion = Lens.field @"functionVersion"
{-# DEPRECATED fcFunctionVersion "Use generic-lens or generic-optics with 'functionVersion' instead." #-}

-- | The name of the @Function@ object.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcName :: Lens.Lens' FunctionConfiguration (Core.Maybe Types.ResourceName)
fcName = Lens.field @"name"
{-# DEPRECATED fcName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The @Function@ request mapping template. Functions support only the 2018-05-29 version of the request mapping template.
--
-- /Note:/ Consider using 'requestMappingTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcRequestMappingTemplate :: Lens.Lens' FunctionConfiguration (Core.Maybe Types.MappingTemplate)
fcRequestMappingTemplate = Lens.field @"requestMappingTemplate"
{-# DEPRECATED fcRequestMappingTemplate "Use generic-lens or generic-optics with 'requestMappingTemplate' instead." #-}

-- | The @Function@ response mapping template.
--
-- /Note:/ Consider using 'responseMappingTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcResponseMappingTemplate :: Lens.Lens' FunctionConfiguration (Core.Maybe Types.MappingTemplate)
fcResponseMappingTemplate = Lens.field @"responseMappingTemplate"
{-# DEPRECATED fcResponseMappingTemplate "Use generic-lens or generic-optics with 'responseMappingTemplate' instead." #-}

instance Core.FromJSON FunctionConfiguration where
  parseJSON =
    Core.withObject "FunctionConfiguration" Core.$
      \x ->
        FunctionConfiguration'
          Core.<$> (x Core..:? "dataSourceName")
          Core.<*> (x Core..:? "description")
          Core.<*> (x Core..:? "functionArn")
          Core.<*> (x Core..:? "functionId")
          Core.<*> (x Core..:? "functionVersion")
          Core.<*> (x Core..:? "name")
          Core.<*> (x Core..:? "requestMappingTemplate")
          Core.<*> (x Core..:? "responseMappingTemplate")
