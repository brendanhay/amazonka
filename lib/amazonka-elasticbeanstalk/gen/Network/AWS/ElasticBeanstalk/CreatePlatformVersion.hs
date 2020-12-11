{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.CreatePlatformVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new version of your custom platform.
module Network.AWS.ElasticBeanstalk.CreatePlatformVersion
  ( -- * Creating a request
    CreatePlatformVersion (..),
    mkCreatePlatformVersion,

    -- ** Request lenses
    cpvOptionSettings,
    cpvEnvironmentName,
    cpvTags,
    cpvPlatformName,
    cpvPlatformVersion,
    cpvPlatformDefinitionBundle,

    -- * Destructuring the response
    CreatePlatformVersionResponse (..),
    mkCreatePlatformVersionResponse,

    -- ** Response lenses
    cpvrsBuilder,
    cpvrsPlatformSummary,
    cpvrsResponseStatus,
  )
where

import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request to create a new platform version.
--
-- /See:/ 'mkCreatePlatformVersion' smart constructor.
data CreatePlatformVersion = CreatePlatformVersion'
  { optionSettings ::
      Lude.Maybe [ConfigurationOptionSetting],
    environmentName :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    platformName :: Lude.Text,
    platformVersion :: Lude.Text,
    platformDefinitionBundle :: S3Location
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreatePlatformVersion' with the minimum fields required to make a request.
--
-- * 'environmentName' - The name of the builder environment.
-- * 'optionSettings' - The configuration option settings to apply to the builder environment.
-- * 'platformDefinitionBundle' - The location of the platform definition archive in Amazon S3.
-- * 'platformName' - The name of your custom platform.
-- * 'platformVersion' - The number, such as 1.0.2, for the new platform version.
-- * 'tags' - Specifies the tags applied to the new platform version.
--
-- Elastic Beanstalk applies these tags only to the platform version. Environments that you create using the platform version don't inherit the tags.
mkCreatePlatformVersion ::
  -- | 'platformName'
  Lude.Text ->
  -- | 'platformVersion'
  Lude.Text ->
  -- | 'platformDefinitionBundle'
  S3Location ->
  CreatePlatformVersion
mkCreatePlatformVersion
  pPlatformName_
  pPlatformVersion_
  pPlatformDefinitionBundle_ =
    CreatePlatformVersion'
      { optionSettings = Lude.Nothing,
        environmentName = Lude.Nothing,
        tags = Lude.Nothing,
        platformName = pPlatformName_,
        platformVersion = pPlatformVersion_,
        platformDefinitionBundle = pPlatformDefinitionBundle_
      }

-- | The configuration option settings to apply to the builder environment.
--
-- /Note:/ Consider using 'optionSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvOptionSettings :: Lens.Lens' CreatePlatformVersion (Lude.Maybe [ConfigurationOptionSetting])
cpvOptionSettings = Lens.lens (optionSettings :: CreatePlatformVersion -> Lude.Maybe [ConfigurationOptionSetting]) (\s a -> s {optionSettings = a} :: CreatePlatformVersion)
{-# DEPRECATED cpvOptionSettings "Use generic-lens or generic-optics with 'optionSettings' instead." #-}

-- | The name of the builder environment.
--
-- /Note:/ Consider using 'environmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvEnvironmentName :: Lens.Lens' CreatePlatformVersion (Lude.Maybe Lude.Text)
cpvEnvironmentName = Lens.lens (environmentName :: CreatePlatformVersion -> Lude.Maybe Lude.Text) (\s a -> s {environmentName = a} :: CreatePlatformVersion)
{-# DEPRECATED cpvEnvironmentName "Use generic-lens or generic-optics with 'environmentName' instead." #-}

-- | Specifies the tags applied to the new platform version.
--
-- Elastic Beanstalk applies these tags only to the platform version. Environments that you create using the platform version don't inherit the tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvTags :: Lens.Lens' CreatePlatformVersion (Lude.Maybe [Tag])
cpvTags = Lens.lens (tags :: CreatePlatformVersion -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreatePlatformVersion)
{-# DEPRECATED cpvTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The name of your custom platform.
--
-- /Note:/ Consider using 'platformName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvPlatformName :: Lens.Lens' CreatePlatformVersion Lude.Text
cpvPlatformName = Lens.lens (platformName :: CreatePlatformVersion -> Lude.Text) (\s a -> s {platformName = a} :: CreatePlatformVersion)
{-# DEPRECATED cpvPlatformName "Use generic-lens or generic-optics with 'platformName' instead." #-}

-- | The number, such as 1.0.2, for the new platform version.
--
-- /Note:/ Consider using 'platformVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvPlatformVersion :: Lens.Lens' CreatePlatformVersion Lude.Text
cpvPlatformVersion = Lens.lens (platformVersion :: CreatePlatformVersion -> Lude.Text) (\s a -> s {platformVersion = a} :: CreatePlatformVersion)
{-# DEPRECATED cpvPlatformVersion "Use generic-lens or generic-optics with 'platformVersion' instead." #-}

-- | The location of the platform definition archive in Amazon S3.
--
-- /Note:/ Consider using 'platformDefinitionBundle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvPlatformDefinitionBundle :: Lens.Lens' CreatePlatformVersion S3Location
cpvPlatformDefinitionBundle = Lens.lens (platformDefinitionBundle :: CreatePlatformVersion -> S3Location) (\s a -> s {platformDefinitionBundle = a} :: CreatePlatformVersion)
{-# DEPRECATED cpvPlatformDefinitionBundle "Use generic-lens or generic-optics with 'platformDefinitionBundle' instead." #-}

instance Lude.AWSRequest CreatePlatformVersion where
  type Rs CreatePlatformVersion = CreatePlatformVersionResponse
  request = Req.postQuery elasticBeanstalkService
  response =
    Res.receiveXMLWrapper
      "CreatePlatformVersionResult"
      ( \s h x ->
          CreatePlatformVersionResponse'
            Lude.<$> (x Lude..@? "Builder")
            Lude.<*> (x Lude..@? "PlatformSummary")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreatePlatformVersion where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreatePlatformVersion where
  toPath = Lude.const "/"

instance Lude.ToQuery CreatePlatformVersion where
  toQuery CreatePlatformVersion' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreatePlatformVersion" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "OptionSettings"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> optionSettings),
        "EnvironmentName" Lude.=: environmentName,
        "Tags"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> tags),
        "PlatformName" Lude.=: platformName,
        "PlatformVersion" Lude.=: platformVersion,
        "PlatformDefinitionBundle" Lude.=: platformDefinitionBundle
      ]

-- | /See:/ 'mkCreatePlatformVersionResponse' smart constructor.
data CreatePlatformVersionResponse = CreatePlatformVersionResponse'
  { builder ::
      Lude.Maybe Builder,
    platformSummary ::
      Lude.Maybe PlatformSummary,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreatePlatformVersionResponse' with the minimum fields required to make a request.
--
-- * 'builder' - The builder used to create the custom platform.
-- * 'platformSummary' - Detailed information about the new version of the custom platform.
-- * 'responseStatus' - The response status code.
mkCreatePlatformVersionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreatePlatformVersionResponse
mkCreatePlatformVersionResponse pResponseStatus_ =
  CreatePlatformVersionResponse'
    { builder = Lude.Nothing,
      platformSummary = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The builder used to create the custom platform.
--
-- /Note:/ Consider using 'builder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvrsBuilder :: Lens.Lens' CreatePlatformVersionResponse (Lude.Maybe Builder)
cpvrsBuilder = Lens.lens (builder :: CreatePlatformVersionResponse -> Lude.Maybe Builder) (\s a -> s {builder = a} :: CreatePlatformVersionResponse)
{-# DEPRECATED cpvrsBuilder "Use generic-lens or generic-optics with 'builder' instead." #-}

-- | Detailed information about the new version of the custom platform.
--
-- /Note:/ Consider using 'platformSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvrsPlatformSummary :: Lens.Lens' CreatePlatformVersionResponse (Lude.Maybe PlatformSummary)
cpvrsPlatformSummary = Lens.lens (platformSummary :: CreatePlatformVersionResponse -> Lude.Maybe PlatformSummary) (\s a -> s {platformSummary = a} :: CreatePlatformVersionResponse)
{-# DEPRECATED cpvrsPlatformSummary "Use generic-lens or generic-optics with 'platformSummary' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpvrsResponseStatus :: Lens.Lens' CreatePlatformVersionResponse Lude.Int
cpvrsResponseStatus = Lens.lens (responseStatus :: CreatePlatformVersionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreatePlatformVersionResponse)
{-# DEPRECATED cpvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
