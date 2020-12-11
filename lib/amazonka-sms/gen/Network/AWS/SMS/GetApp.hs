{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.GetApp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve information about the specified application.
module Network.AWS.SMS.GetApp
  ( -- * Creating a request
    GetApp (..),
    mkGetApp,

    -- ** Request lenses
    gaAppId,

    -- * Destructuring the response
    GetAppResponse (..),
    mkGetAppResponse,

    -- ** Response lenses
    garsAppSummary,
    garsServerGroups,
    garsTags,
    garsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SMS.Types

-- | /See:/ 'mkGetApp' smart constructor.
newtype GetApp = GetApp' {appId :: Lude.Maybe Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetApp' with the minimum fields required to make a request.
--
-- * 'appId' - The ID of the application.
mkGetApp ::
  GetApp
mkGetApp = GetApp' {appId = Lude.Nothing}

-- | The ID of the application.
--
-- /Note:/ Consider using 'appId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaAppId :: Lens.Lens' GetApp (Lude.Maybe Lude.Text)
gaAppId = Lens.lens (appId :: GetApp -> Lude.Maybe Lude.Text) (\s a -> s {appId = a} :: GetApp)
{-# DEPRECATED gaAppId "Use generic-lens or generic-optics with 'appId' instead." #-}

instance Lude.AWSRequest GetApp where
  type Rs GetApp = GetAppResponse
  request = Req.postJSON smsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetAppResponse'
            Lude.<$> (x Lude..?> "appSummary")
            Lude.<*> (x Lude..?> "serverGroups" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "tags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetApp where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSServerMigrationService_V2016_10_24.GetApp" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetApp where
  toJSON GetApp' {..} =
    Lude.object (Lude.catMaybes [("appId" Lude..=) Lude.<$> appId])

instance Lude.ToPath GetApp where
  toPath = Lude.const "/"

instance Lude.ToQuery GetApp where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetAppResponse' smart constructor.
data GetAppResponse = GetAppResponse'
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

-- | Creates a value of 'GetAppResponse' with the minimum fields required to make a request.
--
-- * 'appSummary' - Information about the application.
-- * 'responseStatus' - The response status code.
-- * 'serverGroups' - The server groups that belong to the application.
-- * 'tags' - The tags associated with the application.
mkGetAppResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetAppResponse
mkGetAppResponse pResponseStatus_ =
  GetAppResponse'
    { appSummary = Lude.Nothing,
      serverGroups = Lude.Nothing,
      tags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the application.
--
-- /Note:/ Consider using 'appSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garsAppSummary :: Lens.Lens' GetAppResponse (Lude.Maybe AppSummary)
garsAppSummary = Lens.lens (appSummary :: GetAppResponse -> Lude.Maybe AppSummary) (\s a -> s {appSummary = a} :: GetAppResponse)
{-# DEPRECATED garsAppSummary "Use generic-lens or generic-optics with 'appSummary' instead." #-}

-- | The server groups that belong to the application.
--
-- /Note:/ Consider using 'serverGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garsServerGroups :: Lens.Lens' GetAppResponse (Lude.Maybe [ServerGroup])
garsServerGroups = Lens.lens (serverGroups :: GetAppResponse -> Lude.Maybe [ServerGroup]) (\s a -> s {serverGroups = a} :: GetAppResponse)
{-# DEPRECATED garsServerGroups "Use generic-lens or generic-optics with 'serverGroups' instead." #-}

-- | The tags associated with the application.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garsTags :: Lens.Lens' GetAppResponse (Lude.Maybe [Tag])
garsTags = Lens.lens (tags :: GetAppResponse -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: GetAppResponse)
{-# DEPRECATED garsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garsResponseStatus :: Lens.Lens' GetAppResponse Lude.Int
garsResponseStatus = Lens.lens (responseStatus :: GetAppResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetAppResponse)
{-# DEPRECATED garsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
