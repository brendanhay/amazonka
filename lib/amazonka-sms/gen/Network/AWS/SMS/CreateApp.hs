{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.CreateApp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an application. An application consists of one or more server groups. Each server group contain one or more servers.
module Network.AWS.SMS.CreateApp
  ( -- * Creating a request
    CreateApp (..),
    mkCreateApp,

    -- ** Request lenses
    caClientToken,
    caRoleName,
    caName,
    caDescription,
    caServerGroups,
    caTags,

    -- * Destructuring the response
    CreateAppResponse (..),
    mkCreateAppResponse,

    -- ** Response lenses
    carsAppSummary,
    carsServerGroups,
    carsTags,
    carsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SMS.Types

-- | /See:/ 'mkCreateApp' smart constructor.
data CreateApp = CreateApp'
  { clientToken :: Lude.Maybe Lude.Text,
    roleName :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    serverGroups :: Lude.Maybe [ServerGroup],
    tags :: Lude.Maybe [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateApp' with the minimum fields required to make a request.
--
-- * 'clientToken' - A unique, case-sensitive identifier that you provide to ensure the idempotency of application creation.
-- * 'description' - The description of the new application
-- * 'name' - The name of the new application.
-- * 'roleName' - The name of the service role in the customer's account to be used by AWS SMS.
-- * 'serverGroups' - The server groups to include in the application.
-- * 'tags' - The tags to be associated with the application.
mkCreateApp ::
  CreateApp
mkCreateApp =
  CreateApp'
    { clientToken = Lude.Nothing,
      roleName = Lude.Nothing,
      name = Lude.Nothing,
      description = Lude.Nothing,
      serverGroups = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | A unique, case-sensitive identifier that you provide to ensure the idempotency of application creation.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caClientToken :: Lens.Lens' CreateApp (Lude.Maybe Lude.Text)
caClientToken = Lens.lens (clientToken :: CreateApp -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: CreateApp)
{-# DEPRECATED caClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The name of the service role in the customer's account to be used by AWS SMS.
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caRoleName :: Lens.Lens' CreateApp (Lude.Maybe Lude.Text)
caRoleName = Lens.lens (roleName :: CreateApp -> Lude.Maybe Lude.Text) (\s a -> s {roleName = a} :: CreateApp)
{-# DEPRECATED caRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

-- | The name of the new application.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caName :: Lens.Lens' CreateApp (Lude.Maybe Lude.Text)
caName = Lens.lens (name :: CreateApp -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: CreateApp)
{-# DEPRECATED caName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The description of the new application
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caDescription :: Lens.Lens' CreateApp (Lude.Maybe Lude.Text)
caDescription = Lens.lens (description :: CreateApp -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateApp)
{-# DEPRECATED caDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The server groups to include in the application.
--
-- /Note:/ Consider using 'serverGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caServerGroups :: Lens.Lens' CreateApp (Lude.Maybe [ServerGroup])
caServerGroups = Lens.lens (serverGroups :: CreateApp -> Lude.Maybe [ServerGroup]) (\s a -> s {serverGroups = a} :: CreateApp)
{-# DEPRECATED caServerGroups "Use generic-lens or generic-optics with 'serverGroups' instead." #-}

-- | The tags to be associated with the application.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caTags :: Lens.Lens' CreateApp (Lude.Maybe [Tag])
caTags = Lens.lens (tags :: CreateApp -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateApp)
{-# DEPRECATED caTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateApp where
  type Rs CreateApp = CreateAppResponse
  request = Req.postJSON smsService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateAppResponse'
            Lude.<$> (x Lude..?> "appSummary")
            Lude.<*> (x Lude..?> "serverGroups" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "tags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateApp where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSServerMigrationService_V2016_10_24.CreateApp" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateApp where
  toJSON CreateApp' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("clientToken" Lude..=) Lude.<$> clientToken,
            ("roleName" Lude..=) Lude.<$> roleName,
            ("name" Lude..=) Lude.<$> name,
            ("description" Lude..=) Lude.<$> description,
            ("serverGroups" Lude..=) Lude.<$> serverGroups,
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateApp where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateApp where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateAppResponse' smart constructor.
data CreateAppResponse = CreateAppResponse'
  { appSummary ::
      Lude.Maybe AppSummary,
    serverGroups :: Lude.Maybe [ServerGroup],
    tags :: Lude.Maybe [Tag],
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

-- | Creates a value of 'CreateAppResponse' with the minimum fields required to make a request.
--
-- * 'appSummary' - A summary description of the application.
-- * 'responseStatus' - The response status code.
-- * 'serverGroups' - The server groups included in the application.
-- * 'tags' - The tags associated with the application.
mkCreateAppResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateAppResponse
mkCreateAppResponse pResponseStatus_ =
  CreateAppResponse'
    { appSummary = Lude.Nothing,
      serverGroups = Lude.Nothing,
      tags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A summary description of the application.
--
-- /Note:/ Consider using 'appSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carsAppSummary :: Lens.Lens' CreateAppResponse (Lude.Maybe AppSummary)
carsAppSummary = Lens.lens (appSummary :: CreateAppResponse -> Lude.Maybe AppSummary) (\s a -> s {appSummary = a} :: CreateAppResponse)
{-# DEPRECATED carsAppSummary "Use generic-lens or generic-optics with 'appSummary' instead." #-}

-- | The server groups included in the application.
--
-- /Note:/ Consider using 'serverGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carsServerGroups :: Lens.Lens' CreateAppResponse (Lude.Maybe [ServerGroup])
carsServerGroups = Lens.lens (serverGroups :: CreateAppResponse -> Lude.Maybe [ServerGroup]) (\s a -> s {serverGroups = a} :: CreateAppResponse)
{-# DEPRECATED carsServerGroups "Use generic-lens or generic-optics with 'serverGroups' instead." #-}

-- | The tags associated with the application.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carsTags :: Lens.Lens' CreateAppResponse (Lude.Maybe [Tag])
carsTags = Lens.lens (tags :: CreateAppResponse -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateAppResponse)
{-# DEPRECATED carsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carsResponseStatus :: Lens.Lens' CreateAppResponse Lude.Int
carsResponseStatus = Lens.lens (responseStatus :: CreateAppResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateAppResponse)
{-# DEPRECATED carsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
