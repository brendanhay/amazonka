{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.CreateApplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an application that has one configuration template named @default@ and no application versions.
module Network.AWS.ElasticBeanstalk.CreateApplication
  ( -- * Creating a request
    CreateApplication (..),
    mkCreateApplication,

    -- ** Request lenses
    caApplicationName,
    caResourceLifecycleConfig,
    caDescription,
    caTags,

    -- * Destructuring the response
    ApplicationDescriptionMessage (..),
    mkApplicationDescriptionMessage,

    -- ** Response lenses
    admApplication,
  )
where

import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request to create an application.
--
-- /See:/ 'mkCreateApplication' smart constructor.
data CreateApplication = CreateApplication'
  { -- | The name of the application. Must be unique within your account.
    applicationName :: Lude.Text,
    -- | Specifies an application resource lifecycle configuration to prevent your application from accumulating too many versions.
    resourceLifecycleConfig :: Lude.Maybe ApplicationResourceLifecycleConfig,
    -- | Your description of the application.
    description :: Lude.Maybe Lude.Text,
    -- | Specifies the tags applied to the application.
    --
    -- Elastic Beanstalk applies these tags only to the application. Environments that you create in the application don't inherit the tags.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateApplication' with the minimum fields required to make a request.
--
-- * 'applicationName' - The name of the application. Must be unique within your account.
-- * 'resourceLifecycleConfig' - Specifies an application resource lifecycle configuration to prevent your application from accumulating too many versions.
-- * 'description' - Your description of the application.
-- * 'tags' - Specifies the tags applied to the application.
--
-- Elastic Beanstalk applies these tags only to the application. Environments that you create in the application don't inherit the tags.
mkCreateApplication ::
  -- | 'applicationName'
  Lude.Text ->
  CreateApplication
mkCreateApplication pApplicationName_ =
  CreateApplication'
    { applicationName = pApplicationName_,
      resourceLifecycleConfig = Lude.Nothing,
      description = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The name of the application. Must be unique within your account.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caApplicationName :: Lens.Lens' CreateApplication Lude.Text
caApplicationName = Lens.lens (applicationName :: CreateApplication -> Lude.Text) (\s a -> s {applicationName = a} :: CreateApplication)
{-# DEPRECATED caApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | Specifies an application resource lifecycle configuration to prevent your application from accumulating too many versions.
--
-- /Note:/ Consider using 'resourceLifecycleConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caResourceLifecycleConfig :: Lens.Lens' CreateApplication (Lude.Maybe ApplicationResourceLifecycleConfig)
caResourceLifecycleConfig = Lens.lens (resourceLifecycleConfig :: CreateApplication -> Lude.Maybe ApplicationResourceLifecycleConfig) (\s a -> s {resourceLifecycleConfig = a} :: CreateApplication)
{-# DEPRECATED caResourceLifecycleConfig "Use generic-lens or generic-optics with 'resourceLifecycleConfig' instead." #-}

-- | Your description of the application.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caDescription :: Lens.Lens' CreateApplication (Lude.Maybe Lude.Text)
caDescription = Lens.lens (description :: CreateApplication -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateApplication)
{-# DEPRECATED caDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Specifies the tags applied to the application.
--
-- Elastic Beanstalk applies these tags only to the application. Environments that you create in the application don't inherit the tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caTags :: Lens.Lens' CreateApplication (Lude.Maybe [Tag])
caTags = Lens.lens (tags :: CreateApplication -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateApplication)
{-# DEPRECATED caTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateApplication where
  type Rs CreateApplication = ApplicationDescriptionMessage
  request = Req.postQuery elasticBeanstalkService
  response =
    Res.receiveXMLWrapper
      "CreateApplicationResult"
      (\s h x -> Lude.parseXML x)

instance Lude.ToHeaders CreateApplication where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateApplication where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateApplication where
  toQuery CreateApplication' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateApplication" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "ApplicationName" Lude.=: applicationName,
        "ResourceLifecycleConfig" Lude.=: resourceLifecycleConfig,
        "Description" Lude.=: description,
        "Tags"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> tags)
      ]
