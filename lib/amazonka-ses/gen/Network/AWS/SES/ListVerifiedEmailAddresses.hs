{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.ListVerifiedEmailAddresses
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deprecated. Use the @ListIdentities@ operation to list the email addresses and domains associated with your account.
module Network.AWS.SES.ListVerifiedEmailAddresses
  ( -- * Creating a request
    ListVerifiedEmailAddresses (..),
    mkListVerifiedEmailAddresses,

    -- * Destructuring the response
    ListVerifiedEmailAddressesResponse (..),
    mkListVerifiedEmailAddressesResponse,

    -- ** Response lenses
    lvearsVerifiedEmailAddresses,
    lvearsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | /See:/ 'mkListVerifiedEmailAddresses' smart constructor.
data ListVerifiedEmailAddresses = ListVerifiedEmailAddresses'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListVerifiedEmailAddresses' with the minimum fields required to make a request.
mkListVerifiedEmailAddresses ::
  ListVerifiedEmailAddresses
mkListVerifiedEmailAddresses = ListVerifiedEmailAddresses'

instance Lude.AWSRequest ListVerifiedEmailAddresses where
  type
    Rs ListVerifiedEmailAddresses =
      ListVerifiedEmailAddressesResponse
  request = Req.postQuery sesService
  response =
    Res.receiveXMLWrapper
      "ListVerifiedEmailAddressesResult"
      ( \s h x ->
          ListVerifiedEmailAddressesResponse'
            Lude.<$> ( x Lude..@? "VerifiedEmailAddresses" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListVerifiedEmailAddresses where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListVerifiedEmailAddresses where
  toPath = Lude.const "/"

instance Lude.ToQuery ListVerifiedEmailAddresses where
  toQuery =
    Lude.const
      ( Lude.mconcat
          [ "Action"
              Lude.=: ("ListVerifiedEmailAddresses" :: Lude.ByteString),
            "Version" Lude.=: ("2010-12-01" :: Lude.ByteString)
          ]
      )

-- | A list of email addresses that you have verified with Amazon SES under your AWS account.
--
-- /See:/ 'mkListVerifiedEmailAddressesResponse' smart constructor.
data ListVerifiedEmailAddressesResponse = ListVerifiedEmailAddressesResponse'
  { verifiedEmailAddresses ::
      Lude.Maybe
        [Lude.Text],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListVerifiedEmailAddressesResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'verifiedEmailAddresses' - A list of email addresses that have been verified.
mkListVerifiedEmailAddressesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListVerifiedEmailAddressesResponse
mkListVerifiedEmailAddressesResponse pResponseStatus_ =
  ListVerifiedEmailAddressesResponse'
    { verifiedEmailAddresses =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of email addresses that have been verified.
--
-- /Note:/ Consider using 'verifiedEmailAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvearsVerifiedEmailAddresses :: Lens.Lens' ListVerifiedEmailAddressesResponse (Lude.Maybe [Lude.Text])
lvearsVerifiedEmailAddresses = Lens.lens (verifiedEmailAddresses :: ListVerifiedEmailAddressesResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {verifiedEmailAddresses = a} :: ListVerifiedEmailAddressesResponse)
{-# DEPRECATED lvearsVerifiedEmailAddresses "Use generic-lens or generic-optics with 'verifiedEmailAddresses' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvearsResponseStatus :: Lens.Lens' ListVerifiedEmailAddressesResponse Lude.Int
lvearsResponseStatus = Lens.lens (responseStatus :: ListVerifiedEmailAddressesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListVerifiedEmailAddressesResponse)
{-# DEPRECATED lvearsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
