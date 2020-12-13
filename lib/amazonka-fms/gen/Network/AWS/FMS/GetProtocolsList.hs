{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.GetProtocolsList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified AWS Firewall Manager protocols list.
module Network.AWS.FMS.GetProtocolsList
  ( -- * Creating a request
    GetProtocolsList (..),
    mkGetProtocolsList,

    -- ** Request lenses
    gplDefaultList,
    gplListId,

    -- * Destructuring the response
    GetProtocolsListResponse (..),
    mkGetProtocolsListResponse,

    -- ** Response lenses
    gplrsProtocolsList,
    gplrsProtocolsListARN,
    gplrsResponseStatus,
  )
where

import Network.AWS.FMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetProtocolsList' smart constructor.
data GetProtocolsList = GetProtocolsList'
  { -- | Specifies whether the list to retrieve is a default list owned by AWS Firewall Manager.
    defaultList :: Lude.Maybe Lude.Bool,
    -- | The ID of the AWS Firewall Manager protocols list that you want the details for.
    listId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetProtocolsList' with the minimum fields required to make a request.
--
-- * 'defaultList' - Specifies whether the list to retrieve is a default list owned by AWS Firewall Manager.
-- * 'listId' - The ID of the AWS Firewall Manager protocols list that you want the details for.
mkGetProtocolsList ::
  -- | 'listId'
  Lude.Text ->
  GetProtocolsList
mkGetProtocolsList pListId_ =
  GetProtocolsList' {defaultList = Lude.Nothing, listId = pListId_}

-- | Specifies whether the list to retrieve is a default list owned by AWS Firewall Manager.
--
-- /Note:/ Consider using 'defaultList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gplDefaultList :: Lens.Lens' GetProtocolsList (Lude.Maybe Lude.Bool)
gplDefaultList = Lens.lens (defaultList :: GetProtocolsList -> Lude.Maybe Lude.Bool) (\s a -> s {defaultList = a} :: GetProtocolsList)
{-# DEPRECATED gplDefaultList "Use generic-lens or generic-optics with 'defaultList' instead." #-}

-- | The ID of the AWS Firewall Manager protocols list that you want the details for.
--
-- /Note:/ Consider using 'listId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gplListId :: Lens.Lens' GetProtocolsList Lude.Text
gplListId = Lens.lens (listId :: GetProtocolsList -> Lude.Text) (\s a -> s {listId = a} :: GetProtocolsList)
{-# DEPRECATED gplListId "Use generic-lens or generic-optics with 'listId' instead." #-}

instance Lude.AWSRequest GetProtocolsList where
  type Rs GetProtocolsList = GetProtocolsListResponse
  request = Req.postJSON fmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetProtocolsListResponse'
            Lude.<$> (x Lude..?> "ProtocolsList")
            Lude.<*> (x Lude..?> "ProtocolsListArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetProtocolsList where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSFMS_20180101.GetProtocolsList" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetProtocolsList where
  toJSON GetProtocolsList' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DefaultList" Lude..=) Lude.<$> defaultList,
            Lude.Just ("ListId" Lude..= listId)
          ]
      )

instance Lude.ToPath GetProtocolsList where
  toPath = Lude.const "/"

instance Lude.ToQuery GetProtocolsList where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetProtocolsListResponse' smart constructor.
data GetProtocolsListResponse = GetProtocolsListResponse'
  { -- | Information about the specified AWS Firewall Manager protocols list.
    protocolsList :: Lude.Maybe ProtocolsListData,
    -- | The Amazon Resource Name (ARN) of the specified protocols list.
    protocolsListARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetProtocolsListResponse' with the minimum fields required to make a request.
--
-- * 'protocolsList' - Information about the specified AWS Firewall Manager protocols list.
-- * 'protocolsListARN' - The Amazon Resource Name (ARN) of the specified protocols list.
-- * 'responseStatus' - The response status code.
mkGetProtocolsListResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetProtocolsListResponse
mkGetProtocolsListResponse pResponseStatus_ =
  GetProtocolsListResponse'
    { protocolsList = Lude.Nothing,
      protocolsListARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the specified AWS Firewall Manager protocols list.
--
-- /Note:/ Consider using 'protocolsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gplrsProtocolsList :: Lens.Lens' GetProtocolsListResponse (Lude.Maybe ProtocolsListData)
gplrsProtocolsList = Lens.lens (protocolsList :: GetProtocolsListResponse -> Lude.Maybe ProtocolsListData) (\s a -> s {protocolsList = a} :: GetProtocolsListResponse)
{-# DEPRECATED gplrsProtocolsList "Use generic-lens or generic-optics with 'protocolsList' instead." #-}

-- | The Amazon Resource Name (ARN) of the specified protocols list.
--
-- /Note:/ Consider using 'protocolsListARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gplrsProtocolsListARN :: Lens.Lens' GetProtocolsListResponse (Lude.Maybe Lude.Text)
gplrsProtocolsListARN = Lens.lens (protocolsListARN :: GetProtocolsListResponse -> Lude.Maybe Lude.Text) (\s a -> s {protocolsListARN = a} :: GetProtocolsListResponse)
{-# DEPRECATED gplrsProtocolsListARN "Use generic-lens or generic-optics with 'protocolsListARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gplrsResponseStatus :: Lens.Lens' GetProtocolsListResponse Lude.Int
gplrsResponseStatus = Lens.lens (responseStatus :: GetProtocolsListResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetProtocolsListResponse)
{-# DEPRECATED gplrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
