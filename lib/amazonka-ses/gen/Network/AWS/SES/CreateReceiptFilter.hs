{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.CreateReceiptFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new IP address filter.
--
-- For information about setting up IP address filters, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-ip-filters.html Amazon SES Developer Guide> .
-- You can execute this operation no more than once per second.
module Network.AWS.SES.CreateReceiptFilter
  ( -- * Creating a request
    CreateReceiptFilter (..),
    mkCreateReceiptFilter,

    -- ** Request lenses
    crfFilter,

    -- * Destructuring the response
    CreateReceiptFilterResponse (..),
    mkCreateReceiptFilterResponse,

    -- ** Response lenses
    crfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | Represents a request to create a new IP address filter. You use IP address filters when you receive email with Amazon SES. For more information, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkCreateReceiptFilter' smart constructor.
newtype CreateReceiptFilter = CreateReceiptFilter'
  { -- | A data structure that describes the IP address filter to create, which consists of a name, an IP address range, and whether to allow or block mail from it.
    filter :: ReceiptFilter
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateReceiptFilter' with the minimum fields required to make a request.
--
-- * 'filter' - A data structure that describes the IP address filter to create, which consists of a name, an IP address range, and whether to allow or block mail from it.
mkCreateReceiptFilter ::
  -- | 'filter'
  ReceiptFilter ->
  CreateReceiptFilter
mkCreateReceiptFilter pFilter_ =
  CreateReceiptFilter' {filter = pFilter_}

-- | A data structure that describes the IP address filter to create, which consists of a name, an IP address range, and whether to allow or block mail from it.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crfFilter :: Lens.Lens' CreateReceiptFilter ReceiptFilter
crfFilter = Lens.lens (filter :: CreateReceiptFilter -> ReceiptFilter) (\s a -> s {filter = a} :: CreateReceiptFilter)
{-# DEPRECATED crfFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

instance Lude.AWSRequest CreateReceiptFilter where
  type Rs CreateReceiptFilter = CreateReceiptFilterResponse
  request = Req.postQuery sesService
  response =
    Res.receiveXMLWrapper
      "CreateReceiptFilterResult"
      ( \s h x ->
          CreateReceiptFilterResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateReceiptFilter where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateReceiptFilter where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateReceiptFilter where
  toQuery CreateReceiptFilter' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateReceiptFilter" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "Filter" Lude.=: filter
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'mkCreateReceiptFilterResponse' smart constructor.
newtype CreateReceiptFilterResponse = CreateReceiptFilterResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateReceiptFilterResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkCreateReceiptFilterResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateReceiptFilterResponse
mkCreateReceiptFilterResponse pResponseStatus_ =
  CreateReceiptFilterResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crfrsResponseStatus :: Lens.Lens' CreateReceiptFilterResponse Lude.Int
crfrsResponseStatus = Lens.lens (responseStatus :: CreateReceiptFilterResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateReceiptFilterResponse)
{-# DEPRECATED crfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
