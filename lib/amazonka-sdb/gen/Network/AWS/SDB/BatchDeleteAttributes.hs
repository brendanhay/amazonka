{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.BatchDeleteAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Performs multiple DeleteAttributes operations in a single call, which reduces round trips and latencies. This enables Amazon SimpleDB to optimize requests, which generally yields better throughput.
--
-- The following limitations are enforced for this operation:
--     * 1 MB request size
--
--     * 25 item limit per BatchDeleteAttributes operation
module Network.AWS.SDB.BatchDeleteAttributes
  ( -- * Creating a request
    BatchDeleteAttributes (..),
    mkBatchDeleteAttributes,

    -- ** Request lenses
    bdaDomainName,
    bdaItems,

    -- * Destructuring the response
    BatchDeleteAttributesResponse (..),
    mkBatchDeleteAttributesResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SDB.Types

-- | /See:/ 'mkBatchDeleteAttributes' smart constructor.
data BatchDeleteAttributes = BatchDeleteAttributes'
  { domainName ::
      Lude.Text,
    items :: [DeletableItem]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchDeleteAttributes' with the minimum fields required to make a request.
--
-- * 'domainName' - The name of the domain in which the attributes are being deleted.
-- * 'items' - A list of items on which to perform the operation.
mkBatchDeleteAttributes ::
  -- | 'domainName'
  Lude.Text ->
  BatchDeleteAttributes
mkBatchDeleteAttributes pDomainName_ =
  BatchDeleteAttributes'
    { domainName = pDomainName_,
      items = Lude.mempty
    }

-- | The name of the domain in which the attributes are being deleted.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdaDomainName :: Lens.Lens' BatchDeleteAttributes Lude.Text
bdaDomainName = Lens.lens (domainName :: BatchDeleteAttributes -> Lude.Text) (\s a -> s {domainName = a} :: BatchDeleteAttributes)
{-# DEPRECATED bdaDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | A list of items on which to perform the operation.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdaItems :: Lens.Lens' BatchDeleteAttributes [DeletableItem]
bdaItems = Lens.lens (items :: BatchDeleteAttributes -> [DeletableItem]) (\s a -> s {items = a} :: BatchDeleteAttributes)
{-# DEPRECATED bdaItems "Use generic-lens or generic-optics with 'items' instead." #-}

instance Lude.AWSRequest BatchDeleteAttributes where
  type Rs BatchDeleteAttributes = BatchDeleteAttributesResponse
  request = Req.postQuery sdbService
  response = Res.receiveNull BatchDeleteAttributesResponse'

instance Lude.ToHeaders BatchDeleteAttributes where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath BatchDeleteAttributes where
  toPath = Lude.const "/"

instance Lude.ToQuery BatchDeleteAttributes where
  toQuery BatchDeleteAttributes' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("BatchDeleteAttributes" :: Lude.ByteString),
        "Version" Lude.=: ("2009-04-15" :: Lude.ByteString),
        "DomainName" Lude.=: domainName,
        Lude.toQueryList "Item" items
      ]

-- | /See:/ 'mkBatchDeleteAttributesResponse' smart constructor.
data BatchDeleteAttributesResponse = BatchDeleteAttributesResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchDeleteAttributesResponse' with the minimum fields required to make a request.
mkBatchDeleteAttributesResponse ::
  BatchDeleteAttributesResponse
mkBatchDeleteAttributesResponse = BatchDeleteAttributesResponse'
