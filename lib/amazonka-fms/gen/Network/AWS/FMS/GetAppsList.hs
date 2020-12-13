{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.GetAppsList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified AWS Firewall Manager applications list.
module Network.AWS.FMS.GetAppsList
  ( -- * Creating a request
    GetAppsList (..),
    mkGetAppsList,

    -- ** Request lenses
    galDefaultList,
    galListId,

    -- * Destructuring the response
    GetAppsListResponse (..),
    mkGetAppsListResponse,

    -- ** Response lenses
    galrsAppsListARN,
    galrsAppsList,
    galrsResponseStatus,
  )
where

import Network.AWS.FMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetAppsList' smart constructor.
data GetAppsList = GetAppsList'
  { -- | Specifies whether the list to retrieve is a default list owned by AWS Firewall Manager.
    defaultList :: Lude.Maybe Lude.Bool,
    -- | The ID of the AWS Firewall Manager applications list that you want the details for.
    listId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAppsList' with the minimum fields required to make a request.
--
-- * 'defaultList' - Specifies whether the list to retrieve is a default list owned by AWS Firewall Manager.
-- * 'listId' - The ID of the AWS Firewall Manager applications list that you want the details for.
mkGetAppsList ::
  -- | 'listId'
  Lude.Text ->
  GetAppsList
mkGetAppsList pListId_ =
  GetAppsList' {defaultList = Lude.Nothing, listId = pListId_}

-- | Specifies whether the list to retrieve is a default list owned by AWS Firewall Manager.
--
-- /Note:/ Consider using 'defaultList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
galDefaultList :: Lens.Lens' GetAppsList (Lude.Maybe Lude.Bool)
galDefaultList = Lens.lens (defaultList :: GetAppsList -> Lude.Maybe Lude.Bool) (\s a -> s {defaultList = a} :: GetAppsList)
{-# DEPRECATED galDefaultList "Use generic-lens or generic-optics with 'defaultList' instead." #-}

-- | The ID of the AWS Firewall Manager applications list that you want the details for.
--
-- /Note:/ Consider using 'listId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
galListId :: Lens.Lens' GetAppsList Lude.Text
galListId = Lens.lens (listId :: GetAppsList -> Lude.Text) (\s a -> s {listId = a} :: GetAppsList)
{-# DEPRECATED galListId "Use generic-lens or generic-optics with 'listId' instead." #-}

instance Lude.AWSRequest GetAppsList where
  type Rs GetAppsList = GetAppsListResponse
  request = Req.postJSON fmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetAppsListResponse'
            Lude.<$> (x Lude..?> "AppsListArn")
            Lude.<*> (x Lude..?> "AppsList")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetAppsList where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSFMS_20180101.GetAppsList" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetAppsList where
  toJSON GetAppsList' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DefaultList" Lude..=) Lude.<$> defaultList,
            Lude.Just ("ListId" Lude..= listId)
          ]
      )

instance Lude.ToPath GetAppsList where
  toPath = Lude.const "/"

instance Lude.ToQuery GetAppsList where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetAppsListResponse' smart constructor.
data GetAppsListResponse = GetAppsListResponse'
  { -- | The Amazon Resource Name (ARN) of the applications list.
    appsListARN :: Lude.Maybe Lude.Text,
    -- | Information about the specified AWS Firewall Manager applications list.
    appsList :: Lude.Maybe AppsListData,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAppsListResponse' with the minimum fields required to make a request.
--
-- * 'appsListARN' - The Amazon Resource Name (ARN) of the applications list.
-- * 'appsList' - Information about the specified AWS Firewall Manager applications list.
-- * 'responseStatus' - The response status code.
mkGetAppsListResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetAppsListResponse
mkGetAppsListResponse pResponseStatus_ =
  GetAppsListResponse'
    { appsListARN = Lude.Nothing,
      appsList = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the applications list.
--
-- /Note:/ Consider using 'appsListARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
galrsAppsListARN :: Lens.Lens' GetAppsListResponse (Lude.Maybe Lude.Text)
galrsAppsListARN = Lens.lens (appsListARN :: GetAppsListResponse -> Lude.Maybe Lude.Text) (\s a -> s {appsListARN = a} :: GetAppsListResponse)
{-# DEPRECATED galrsAppsListARN "Use generic-lens or generic-optics with 'appsListARN' instead." #-}

-- | Information about the specified AWS Firewall Manager applications list.
--
-- /Note:/ Consider using 'appsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
galrsAppsList :: Lens.Lens' GetAppsListResponse (Lude.Maybe AppsListData)
galrsAppsList = Lens.lens (appsList :: GetAppsListResponse -> Lude.Maybe AppsListData) (\s a -> s {appsList = a} :: GetAppsListResponse)
{-# DEPRECATED galrsAppsList "Use generic-lens or generic-optics with 'appsList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
galrsResponseStatus :: Lens.Lens' GetAppsListResponse Lude.Int
galrsResponseStatus = Lens.lens (responseStatus :: GetAppsListResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetAppsListResponse)
{-# DEPRECATED galrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
