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
    fcFunctionARN,
    fcDataSourceName,
    fcRequestMappingTemplate,
    fcName,
    fcFunctionId,
    fcResponseMappingTemplate,
    fcFunctionVersion,
    fcDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A function is a reusable entity. Multiple functions can be used to compose the resolver logic.
--
-- /See:/ 'mkFunctionConfiguration' smart constructor.
data FunctionConfiguration = FunctionConfiguration'
  { -- | The ARN of the @Function@ object.
    functionARN :: Lude.Maybe Lude.Text,
    -- | The name of the @DataSource@ .
    dataSourceName :: Lude.Maybe Lude.Text,
    -- | The @Function@ request mapping template. Functions support only the 2018-05-29 version of the request mapping template.
    requestMappingTemplate :: Lude.Maybe Lude.Text,
    -- | The name of the @Function@ object.
    name :: Lude.Maybe Lude.Text,
    -- | A unique ID representing the @Function@ object.
    functionId :: Lude.Maybe Lude.Text,
    -- | The @Function@ response mapping template.
    responseMappingTemplate :: Lude.Maybe Lude.Text,
    -- | The version of the request mapping template. Currently only the 2018-05-29 version of the template is supported.
    functionVersion :: Lude.Maybe Lude.Text,
    -- | The @Function@ description.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FunctionConfiguration' with the minimum fields required to make a request.
--
-- * 'functionARN' - The ARN of the @Function@ object.
-- * 'dataSourceName' - The name of the @DataSource@ .
-- * 'requestMappingTemplate' - The @Function@ request mapping template. Functions support only the 2018-05-29 version of the request mapping template.
-- * 'name' - The name of the @Function@ object.
-- * 'functionId' - A unique ID representing the @Function@ object.
-- * 'responseMappingTemplate' - The @Function@ response mapping template.
-- * 'functionVersion' - The version of the request mapping template. Currently only the 2018-05-29 version of the template is supported.
-- * 'description' - The @Function@ description.
mkFunctionConfiguration ::
  FunctionConfiguration
mkFunctionConfiguration =
  FunctionConfiguration'
    { functionARN = Lude.Nothing,
      dataSourceName = Lude.Nothing,
      requestMappingTemplate = Lude.Nothing,
      name = Lude.Nothing,
      functionId = Lude.Nothing,
      responseMappingTemplate = Lude.Nothing,
      functionVersion = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The ARN of the @Function@ object.
--
-- /Note:/ Consider using 'functionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcFunctionARN :: Lens.Lens' FunctionConfiguration (Lude.Maybe Lude.Text)
fcFunctionARN = Lens.lens (functionARN :: FunctionConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {functionARN = a} :: FunctionConfiguration)
{-# DEPRECATED fcFunctionARN "Use generic-lens or generic-optics with 'functionARN' instead." #-}

-- | The name of the @DataSource@ .
--
-- /Note:/ Consider using 'dataSourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcDataSourceName :: Lens.Lens' FunctionConfiguration (Lude.Maybe Lude.Text)
fcDataSourceName = Lens.lens (dataSourceName :: FunctionConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {dataSourceName = a} :: FunctionConfiguration)
{-# DEPRECATED fcDataSourceName "Use generic-lens or generic-optics with 'dataSourceName' instead." #-}

-- | The @Function@ request mapping template. Functions support only the 2018-05-29 version of the request mapping template.
--
-- /Note:/ Consider using 'requestMappingTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcRequestMappingTemplate :: Lens.Lens' FunctionConfiguration (Lude.Maybe Lude.Text)
fcRequestMappingTemplate = Lens.lens (requestMappingTemplate :: FunctionConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {requestMappingTemplate = a} :: FunctionConfiguration)
{-# DEPRECATED fcRequestMappingTemplate "Use generic-lens or generic-optics with 'requestMappingTemplate' instead." #-}

-- | The name of the @Function@ object.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcName :: Lens.Lens' FunctionConfiguration (Lude.Maybe Lude.Text)
fcName = Lens.lens (name :: FunctionConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: FunctionConfiguration)
{-# DEPRECATED fcName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A unique ID representing the @Function@ object.
--
-- /Note:/ Consider using 'functionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcFunctionId :: Lens.Lens' FunctionConfiguration (Lude.Maybe Lude.Text)
fcFunctionId = Lens.lens (functionId :: FunctionConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {functionId = a} :: FunctionConfiguration)
{-# DEPRECATED fcFunctionId "Use generic-lens or generic-optics with 'functionId' instead." #-}

-- | The @Function@ response mapping template.
--
-- /Note:/ Consider using 'responseMappingTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcResponseMappingTemplate :: Lens.Lens' FunctionConfiguration (Lude.Maybe Lude.Text)
fcResponseMappingTemplate = Lens.lens (responseMappingTemplate :: FunctionConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {responseMappingTemplate = a} :: FunctionConfiguration)
{-# DEPRECATED fcResponseMappingTemplate "Use generic-lens or generic-optics with 'responseMappingTemplate' instead." #-}

-- | The version of the request mapping template. Currently only the 2018-05-29 version of the template is supported.
--
-- /Note:/ Consider using 'functionVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcFunctionVersion :: Lens.Lens' FunctionConfiguration (Lude.Maybe Lude.Text)
fcFunctionVersion = Lens.lens (functionVersion :: FunctionConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {functionVersion = a} :: FunctionConfiguration)
{-# DEPRECATED fcFunctionVersion "Use generic-lens or generic-optics with 'functionVersion' instead." #-}

-- | The @Function@ description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcDescription :: Lens.Lens' FunctionConfiguration (Lude.Maybe Lude.Text)
fcDescription = Lens.lens (description :: FunctionConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: FunctionConfiguration)
{-# DEPRECATED fcDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON FunctionConfiguration where
  parseJSON =
    Lude.withObject
      "FunctionConfiguration"
      ( \x ->
          FunctionConfiguration'
            Lude.<$> (x Lude..:? "functionArn")
            Lude.<*> (x Lude..:? "dataSourceName")
            Lude.<*> (x Lude..:? "requestMappingTemplate")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "functionId")
            Lude.<*> (x Lude..:? "responseMappingTemplate")
            Lude.<*> (x Lude..:? "functionVersion")
            Lude.<*> (x Lude..:? "description")
      )
