{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.PutProtocolsList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS Firewall Manager protocols list.
module Network.AWS.FMS.PutProtocolsList
  ( -- * Creating a request
    PutProtocolsList (..),
    mkPutProtocolsList,

    -- ** Request lenses
    pplProtocolsList,
    pplTagList,

    -- * Destructuring the response
    PutProtocolsListResponse (..),
    mkPutProtocolsListResponse,

    -- ** Response lenses
    pplrsProtocolsList,
    pplrsProtocolsListARN,
    pplrsResponseStatus,
  )
where

import Network.AWS.FMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutProtocolsList' smart constructor.
data PutProtocolsList = PutProtocolsList'
  { -- | The details of the AWS Firewall Manager protocols list to be created.
    protocolsList :: ProtocolsListData,
    -- | The tags associated with the resource.
    tagList :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutProtocolsList' with the minimum fields required to make a request.
--
-- * 'protocolsList' - The details of the AWS Firewall Manager protocols list to be created.
-- * 'tagList' - The tags associated with the resource.
mkPutProtocolsList ::
  -- | 'protocolsList'
  ProtocolsListData ->
  PutProtocolsList
mkPutProtocolsList pProtocolsList_ =
  PutProtocolsList'
    { protocolsList = pProtocolsList_,
      tagList = Lude.Nothing
    }

-- | The details of the AWS Firewall Manager protocols list to be created.
--
-- /Note:/ Consider using 'protocolsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pplProtocolsList :: Lens.Lens' PutProtocolsList ProtocolsListData
pplProtocolsList = Lens.lens (protocolsList :: PutProtocolsList -> ProtocolsListData) (\s a -> s {protocolsList = a} :: PutProtocolsList)
{-# DEPRECATED pplProtocolsList "Use generic-lens or generic-optics with 'protocolsList' instead." #-}

-- | The tags associated with the resource.
--
-- /Note:/ Consider using 'tagList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pplTagList :: Lens.Lens' PutProtocolsList (Lude.Maybe [Tag])
pplTagList = Lens.lens (tagList :: PutProtocolsList -> Lude.Maybe [Tag]) (\s a -> s {tagList = a} :: PutProtocolsList)
{-# DEPRECATED pplTagList "Use generic-lens or generic-optics with 'tagList' instead." #-}

instance Lude.AWSRequest PutProtocolsList where
  type Rs PutProtocolsList = PutProtocolsListResponse
  request = Req.postJSON fmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutProtocolsListResponse'
            Lude.<$> (x Lude..?> "ProtocolsList")
            Lude.<*> (x Lude..?> "ProtocolsListArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutProtocolsList where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSFMS_20180101.PutProtocolsList" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutProtocolsList where
  toJSON PutProtocolsList' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ProtocolsList" Lude..= protocolsList),
            ("TagList" Lude..=) Lude.<$> tagList
          ]
      )

instance Lude.ToPath PutProtocolsList where
  toPath = Lude.const "/"

instance Lude.ToQuery PutProtocolsList where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutProtocolsListResponse' smart constructor.
data PutProtocolsListResponse = PutProtocolsListResponse'
  { -- | The details of the AWS Firewall Manager protocols list.
    protocolsList :: Lude.Maybe ProtocolsListData,
    -- | The Amazon Resource Name (ARN) of the protocols list.
    protocolsListARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutProtocolsListResponse' with the minimum fields required to make a request.
--
-- * 'protocolsList' - The details of the AWS Firewall Manager protocols list.
-- * 'protocolsListARN' - The Amazon Resource Name (ARN) of the protocols list.
-- * 'responseStatus' - The response status code.
mkPutProtocolsListResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutProtocolsListResponse
mkPutProtocolsListResponse pResponseStatus_ =
  PutProtocolsListResponse'
    { protocolsList = Lude.Nothing,
      protocolsListARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The details of the AWS Firewall Manager protocols list.
--
-- /Note:/ Consider using 'protocolsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pplrsProtocolsList :: Lens.Lens' PutProtocolsListResponse (Lude.Maybe ProtocolsListData)
pplrsProtocolsList = Lens.lens (protocolsList :: PutProtocolsListResponse -> Lude.Maybe ProtocolsListData) (\s a -> s {protocolsList = a} :: PutProtocolsListResponse)
{-# DEPRECATED pplrsProtocolsList "Use generic-lens or generic-optics with 'protocolsList' instead." #-}

-- | The Amazon Resource Name (ARN) of the protocols list.
--
-- /Note:/ Consider using 'protocolsListARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pplrsProtocolsListARN :: Lens.Lens' PutProtocolsListResponse (Lude.Maybe Lude.Text)
pplrsProtocolsListARN = Lens.lens (protocolsListARN :: PutProtocolsListResponse -> Lude.Maybe Lude.Text) (\s a -> s {protocolsListARN = a} :: PutProtocolsListResponse)
{-# DEPRECATED pplrsProtocolsListARN "Use generic-lens or generic-optics with 'protocolsListARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pplrsResponseStatus :: Lens.Lens' PutProtocolsListResponse Lude.Int
pplrsResponseStatus = Lens.lens (responseStatus :: PutProtocolsListResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutProtocolsListResponse)
{-# DEPRECATED pplrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
