{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.UpdateApp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified application.
module Network.AWS.SMS.UpdateApp
  ( -- * Creating a request
    UpdateApp (..),
    mkUpdateApp,

    -- ** Request lenses
    uaRoleName,
    uaAppId,
    uaName,
    uaDescription,
    uaServerGroups,
    uaTags,

    -- * Destructuring the response
    UpdateAppResponse (..),
    mkUpdateAppResponse,

    -- ** Response lenses
    uarsAppSummary,
    uarsServerGroups,
    uarsTags,
    uarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SMS.Types

-- | /See:/ 'mkUpdateApp' smart constructor.
data UpdateApp = UpdateApp'
  { roleName :: Lude.Maybe Lude.Text,
    appId :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'UpdateApp' with the minimum fields required to make a request.
--
-- * 'appId' - The ID of the application.
-- * 'description' - The new description of the application.
-- * 'name' - The new name of the application.
-- * 'roleName' - The name of the service role in the customer's account used by AWS SMS.
-- * 'serverGroups' - The server groups in the application to update.
-- * 'tags' - The tags to associate with the application.
mkUpdateApp ::
  UpdateApp
mkUpdateApp =
  UpdateApp'
    { roleName = Lude.Nothing,
      appId = Lude.Nothing,
      name = Lude.Nothing,
      description = Lude.Nothing,
      serverGroups = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The name of the service role in the customer's account used by AWS SMS.
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaRoleName :: Lens.Lens' UpdateApp (Lude.Maybe Lude.Text)
uaRoleName = Lens.lens (roleName :: UpdateApp -> Lude.Maybe Lude.Text) (\s a -> s {roleName = a} :: UpdateApp)
{-# DEPRECATED uaRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

-- | The ID of the application.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaAppId :: Lens.Lens' UpdateApp (Lude.Maybe Lude.Text)
uaAppId = Lens.lens (appId :: UpdateApp -> Lude.Maybe Lude.Text) (\s a -> s {appId = a} :: UpdateApp)
{-# DEPRECATED uaAppId "Use generic-lens or generic-optics with 'appId' instead." #-}

-- | The new name of the application.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaName :: Lens.Lens' UpdateApp (Lude.Maybe Lude.Text)
uaName = Lens.lens (name :: UpdateApp -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateApp)
{-# DEPRECATED uaName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The new description of the application.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaDescription :: Lens.Lens' UpdateApp (Lude.Maybe Lude.Text)
uaDescription = Lens.lens (description :: UpdateApp -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateApp)
{-# DEPRECATED uaDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The server groups in the application to update.
--
-- /Note:/ Consider using 'serverGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaServerGroups :: Lens.Lens' UpdateApp (Lude.Maybe [ServerGroup])
uaServerGroups = Lens.lens (serverGroups :: UpdateApp -> Lude.Maybe [ServerGroup]) (\s a -> s {serverGroups = a} :: UpdateApp)
{-# DEPRECATED uaServerGroups "Use generic-lens or generic-optics with 'serverGroups' instead." #-}

-- | The tags to associate with the application.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaTags :: Lens.Lens' UpdateApp (Lude.Maybe [Tag])
uaTags = Lens.lens (tags :: UpdateApp -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: UpdateApp)
{-# DEPRECATED uaTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest UpdateApp where
  type Rs UpdateApp = UpdateAppResponse
  request = Req.postJSON smsService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateAppResponse'
            Lude.<$> (x Lude..?> "appSummary")
            Lude.<*> (x Lude..?> "serverGroups" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "tags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateApp where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSServerMigrationService_V2016_10_24.UpdateApp" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateApp where
  toJSON UpdateApp' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("roleName" Lude..=) Lude.<$> roleName,
            ("appId" Lude..=) Lude.<$> appId,
            ("name" Lude..=) Lude.<$> name,
            ("description" Lude..=) Lude.<$> description,
            ("serverGroups" Lude..=) Lude.<$> serverGroups,
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath UpdateApp where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateApp where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateAppResponse' smart constructor.
data UpdateAppResponse = UpdateAppResponse'
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

-- | Creates a value of 'UpdateAppResponse' with the minimum fields required to make a request.
--
-- * 'appSummary' - A summary description of the application.
-- * 'responseStatus' - The response status code.
-- * 'serverGroups' - The updated server groups in the application.
-- * 'tags' - The tags associated with the application.
mkUpdateAppResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateAppResponse
mkUpdateAppResponse pResponseStatus_ =
  UpdateAppResponse'
    { appSummary = Lude.Nothing,
      serverGroups = Lude.Nothing,
      tags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A summary description of the application.
--
-- /Note:/ Consider using 'appSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarsAppSummary :: Lens.Lens' UpdateAppResponse (Lude.Maybe AppSummary)
uarsAppSummary = Lens.lens (appSummary :: UpdateAppResponse -> Lude.Maybe AppSummary) (\s a -> s {appSummary = a} :: UpdateAppResponse)
{-# DEPRECATED uarsAppSummary "Use generic-lens or generic-optics with 'appSummary' instead." #-}

-- | The updated server groups in the application.
--
-- /Note:/ Consider using 'serverGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarsServerGroups :: Lens.Lens' UpdateAppResponse (Lude.Maybe [ServerGroup])
uarsServerGroups = Lens.lens (serverGroups :: UpdateAppResponse -> Lude.Maybe [ServerGroup]) (\s a -> s {serverGroups = a} :: UpdateAppResponse)
{-# DEPRECATED uarsServerGroups "Use generic-lens or generic-optics with 'serverGroups' instead." #-}

-- | The tags associated with the application.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarsTags :: Lens.Lens' UpdateAppResponse (Lude.Maybe [Tag])
uarsTags = Lens.lens (tags :: UpdateAppResponse -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: UpdateAppResponse)
{-# DEPRECATED uarsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarsResponseStatus :: Lens.Lens' UpdateAppResponse Lude.Int
uarsResponseStatus = Lens.lens (responseStatus :: UpdateAppResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateAppResponse)
{-# DEPRECATED uarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
