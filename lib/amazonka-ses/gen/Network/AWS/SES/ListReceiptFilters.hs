{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.ListReceiptFilters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the IP address filters associated with your AWS account in the current AWS Region.
--
-- For information about managing IP address filters, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-managing-ip-filters.html Amazon SES Developer Guide> .
-- You can execute this operation no more than once per second.
module Network.AWS.SES.ListReceiptFilters
  ( -- * Creating a request
    ListReceiptFilters (..),
    mkListReceiptFilters,

    -- * Destructuring the response
    ListReceiptFiltersResponse (..),
    mkListReceiptFiltersResponse,

    -- ** Response lenses
    lrfrsFilters,
    lrfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | Represents a request to list the IP address filters that exist under your AWS account. You use IP address filters when you receive email with Amazon SES. For more information, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-concepts.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkListReceiptFilters' smart constructor.
data ListReceiptFilters = ListReceiptFilters'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListReceiptFilters' with the minimum fields required to make a request.
mkListReceiptFilters ::
  ListReceiptFilters
mkListReceiptFilters = ListReceiptFilters'

instance Lude.AWSRequest ListReceiptFilters where
  type Rs ListReceiptFilters = ListReceiptFiltersResponse
  request = Req.postQuery sesService
  response =
    Res.receiveXMLWrapper
      "ListReceiptFiltersResult"
      ( \s h x ->
          ListReceiptFiltersResponse'
            Lude.<$> ( x Lude..@? "Filters" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListReceiptFilters where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListReceiptFilters where
  toPath = Lude.const "/"

instance Lude.ToQuery ListReceiptFilters where
  toQuery =
    Lude.const
      ( Lude.mconcat
          [ "Action" Lude.=: ("ListReceiptFilters" :: Lude.ByteString),
            "Version" Lude.=: ("2010-12-01" :: Lude.ByteString)
          ]
      )

-- | A list of IP address filters that exist under your AWS account.
--
-- /See:/ 'mkListReceiptFiltersResponse' smart constructor.
data ListReceiptFiltersResponse = ListReceiptFiltersResponse'
  { -- | A list of IP address filter data structures, which each consist of a name, an IP address range, and whether to allow or block mail from it.
    filters :: Lude.Maybe [ReceiptFilter],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListReceiptFiltersResponse' with the minimum fields required to make a request.
--
-- * 'filters' - A list of IP address filter data structures, which each consist of a name, an IP address range, and whether to allow or block mail from it.
-- * 'responseStatus' - The response status code.
mkListReceiptFiltersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListReceiptFiltersResponse
mkListReceiptFiltersResponse pResponseStatus_ =
  ListReceiptFiltersResponse'
    { filters = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of IP address filter data structures, which each consist of a name, an IP address range, and whether to allow or block mail from it.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrfrsFilters :: Lens.Lens' ListReceiptFiltersResponse (Lude.Maybe [ReceiptFilter])
lrfrsFilters = Lens.lens (filters :: ListReceiptFiltersResponse -> Lude.Maybe [ReceiptFilter]) (\s a -> s {filters = a} :: ListReceiptFiltersResponse)
{-# DEPRECATED lrfrsFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrfrsResponseStatus :: Lens.Lens' ListReceiptFiltersResponse Lude.Int
lrfrsResponseStatus = Lens.lens (responseStatus :: ListReceiptFiltersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListReceiptFiltersResponse)
{-# DEPRECATED lrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
