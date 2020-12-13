{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.ListRemoteAccessSessions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all currently running remote access sessions.
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.ListRemoteAccessSessions
  ( -- * Creating a request
    ListRemoteAccessSessions (..),
    mkListRemoteAccessSessions,

    -- ** Request lenses
    lrasArn,
    lrasNextToken,

    -- * Destructuring the response
    ListRemoteAccessSessionsResponse (..),
    mkListRemoteAccessSessionsResponse,

    -- ** Response lenses
    lrasrsNextToken,
    lrasrsRemoteAccessSessions,
    lrasrsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the request to return information about the remote access session.
--
-- /See:/ 'mkListRemoteAccessSessions' smart constructor.
data ListRemoteAccessSessions = ListRemoteAccessSessions'
  { -- | The Amazon Resource Name (ARN) of the project about which you are requesting information.
    arn :: Lude.Text,
    -- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
    nextToken :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListRemoteAccessSessions' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the project about which you are requesting information.
-- * 'nextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
mkListRemoteAccessSessions ::
  -- | 'arn'
  Lude.Text ->
  ListRemoteAccessSessions
mkListRemoteAccessSessions pArn_ =
  ListRemoteAccessSessions' {arn = pArn_, nextToken = Lude.Nothing}

-- | The Amazon Resource Name (ARN) of the project about which you are requesting information.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrasArn :: Lens.Lens' ListRemoteAccessSessions Lude.Text
lrasArn = Lens.lens (arn :: ListRemoteAccessSessions -> Lude.Text) (\s a -> s {arn = a} :: ListRemoteAccessSessions)
{-# DEPRECATED lrasArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrasNextToken :: Lens.Lens' ListRemoteAccessSessions (Lude.Maybe Lude.Text)
lrasNextToken = Lens.lens (nextToken :: ListRemoteAccessSessions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListRemoteAccessSessions)
{-# DEPRECATED lrasNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Page.AWSPager ListRemoteAccessSessions where
  page rq rs
    | Page.stop (rs Lens.^. lrasrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lrasrsRemoteAccessSessions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lrasNextToken Lens..~ rs Lens.^. lrasrsNextToken

instance Lude.AWSRequest ListRemoteAccessSessions where
  type Rs ListRemoteAccessSessions = ListRemoteAccessSessionsResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListRemoteAccessSessionsResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "remoteAccessSessions" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListRemoteAccessSessions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "DeviceFarm_20150623.ListRemoteAccessSessions" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListRemoteAccessSessions where
  toJSON ListRemoteAccessSessions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("arn" Lude..= arn),
            ("nextToken" Lude..=) Lude.<$> nextToken
          ]
      )

instance Lude.ToPath ListRemoteAccessSessions where
  toPath = Lude.const "/"

instance Lude.ToQuery ListRemoteAccessSessions where
  toQuery = Lude.const Lude.mempty

-- | Represents the response from the server after AWS Device Farm makes a request to return information about the remote access session.
--
-- /See:/ 'mkListRemoteAccessSessionsResponse' smart constructor.
data ListRemoteAccessSessionsResponse = ListRemoteAccessSessionsResponse'
  { -- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A container that represents the metadata from the service about each remote access session you are requesting.
    remoteAccessSessions :: Lude.Maybe [RemoteAccessSession],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListRemoteAccessSessionsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
-- * 'remoteAccessSessions' - A container that represents the metadata from the service about each remote access session you are requesting.
-- * 'responseStatus' - The response status code.
mkListRemoteAccessSessionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListRemoteAccessSessionsResponse
mkListRemoteAccessSessionsResponse pResponseStatus_ =
  ListRemoteAccessSessionsResponse'
    { nextToken = Lude.Nothing,
      remoteAccessSessions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrasrsNextToken :: Lens.Lens' ListRemoteAccessSessionsResponse (Lude.Maybe Lude.Text)
lrasrsNextToken = Lens.lens (nextToken :: ListRemoteAccessSessionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListRemoteAccessSessionsResponse)
{-# DEPRECATED lrasrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A container that represents the metadata from the service about each remote access session you are requesting.
--
-- /Note:/ Consider using 'remoteAccessSessions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrasrsRemoteAccessSessions :: Lens.Lens' ListRemoteAccessSessionsResponse (Lude.Maybe [RemoteAccessSession])
lrasrsRemoteAccessSessions = Lens.lens (remoteAccessSessions :: ListRemoteAccessSessionsResponse -> Lude.Maybe [RemoteAccessSession]) (\s a -> s {remoteAccessSessions = a} :: ListRemoteAccessSessionsResponse)
{-# DEPRECATED lrasrsRemoteAccessSessions "Use generic-lens or generic-optics with 'remoteAccessSessions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrasrsResponseStatus :: Lens.Lens' ListRemoteAccessSessionsResponse Lude.Int
lrasrsResponseStatus = Lens.lens (responseStatus :: ListRemoteAccessSessionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListRemoteAccessSessionsResponse)
{-# DEPRECATED lrasrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
