{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.PutAppsList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS Firewall Manager applications list.
module Network.AWS.FMS.PutAppsList
  ( -- * Creating a request
    PutAppsList (..),
    mkPutAppsList,

    -- ** Request lenses
    palTagList,
    palAppsList,

    -- * Destructuring the response
    PutAppsListResponse (..),
    mkPutAppsListResponse,

    -- ** Response lenses
    palrsAppsListARN,
    palrsAppsList,
    palrsResponseStatus,
  )
where

import Network.AWS.FMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutAppsList' smart constructor.
data PutAppsList = PutAppsList'
  { tagList :: Lude.Maybe [Tag],
    appsList :: AppsListData
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutAppsList' with the minimum fields required to make a request.
--
-- * 'appsList' - The details of the AWS Firewall Manager applications list to be created.
-- * 'tagList' - The tags associated with the resource.
mkPutAppsList ::
  -- | 'appsList'
  AppsListData ->
  PutAppsList
mkPutAppsList pAppsList_ =
  PutAppsList' {tagList = Lude.Nothing, appsList = pAppsList_}

-- | The tags associated with the resource.
--
-- /Note:/ Consider using 'tagList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
palTagList :: Lens.Lens' PutAppsList (Lude.Maybe [Tag])
palTagList = Lens.lens (tagList :: PutAppsList -> Lude.Maybe [Tag]) (\s a -> s {tagList = a} :: PutAppsList)
{-# DEPRECATED palTagList "Use generic-lens or generic-optics with 'tagList' instead." #-}

-- | The details of the AWS Firewall Manager applications list to be created.
--
-- /Note:/ Consider using 'appsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
palAppsList :: Lens.Lens' PutAppsList AppsListData
palAppsList = Lens.lens (appsList :: PutAppsList -> AppsListData) (\s a -> s {appsList = a} :: PutAppsList)
{-# DEPRECATED palAppsList "Use generic-lens or generic-optics with 'appsList' instead." #-}

instance Lude.AWSRequest PutAppsList where
  type Rs PutAppsList = PutAppsListResponse
  request = Req.postJSON fmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutAppsListResponse'
            Lude.<$> (x Lude..?> "AppsListArn")
            Lude.<*> (x Lude..?> "AppsList")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutAppsList where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSFMS_20180101.PutAppsList" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutAppsList where
  toJSON PutAppsList' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("TagList" Lude..=) Lude.<$> tagList,
            Lude.Just ("AppsList" Lude..= appsList)
          ]
      )

instance Lude.ToPath PutAppsList where
  toPath = Lude.const "/"

instance Lude.ToQuery PutAppsList where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutAppsListResponse' smart constructor.
data PutAppsListResponse = PutAppsListResponse'
  { appsListARN ::
      Lude.Maybe Lude.Text,
    appsList :: Lude.Maybe AppsListData,
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

-- | Creates a value of 'PutAppsListResponse' with the minimum fields required to make a request.
--
-- * 'appsList' - The details of the AWS Firewall Manager applications list.
-- * 'appsListARN' - The Amazon Resource Name (ARN) of the applications list.
-- * 'responseStatus' - The response status code.
mkPutAppsListResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutAppsListResponse
mkPutAppsListResponse pResponseStatus_ =
  PutAppsListResponse'
    { appsListARN = Lude.Nothing,
      appsList = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the applications list.
--
-- /Note:/ Consider using 'appsListARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
palrsAppsListARN :: Lens.Lens' PutAppsListResponse (Lude.Maybe Lude.Text)
palrsAppsListARN = Lens.lens (appsListARN :: PutAppsListResponse -> Lude.Maybe Lude.Text) (\s a -> s {appsListARN = a} :: PutAppsListResponse)
{-# DEPRECATED palrsAppsListARN "Use generic-lens or generic-optics with 'appsListARN' instead." #-}

-- | The details of the AWS Firewall Manager applications list.
--
-- /Note:/ Consider using 'appsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
palrsAppsList :: Lens.Lens' PutAppsListResponse (Lude.Maybe AppsListData)
palrsAppsList = Lens.lens (appsList :: PutAppsListResponse -> Lude.Maybe AppsListData) (\s a -> s {appsList = a} :: PutAppsListResponse)
{-# DEPRECATED palrsAppsList "Use generic-lens or generic-optics with 'appsList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
palrsResponseStatus :: Lens.Lens' PutAppsListResponse Lude.Int
palrsResponseStatus = Lens.lens (responseStatus :: PutAppsListResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutAppsListResponse)
{-# DEPRECATED palrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
