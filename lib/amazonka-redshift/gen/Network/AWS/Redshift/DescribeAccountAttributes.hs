{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeAccountAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of attributes attached to an account
module Network.AWS.Redshift.DescribeAccountAttributes
  ( -- * Creating a request
    DescribeAccountAttributes (..),
    mkDescribeAccountAttributes,

    -- ** Request lenses
    daaAttributeNames,

    -- * Destructuring the response
    DescribeAccountAttributesResponse (..),
    mkDescribeAccountAttributesResponse,

    -- ** Response lenses
    daarsAccountAttributes,
    daarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeAccountAttributes' smart constructor.
newtype DescribeAccountAttributes = DescribeAccountAttributes'
  { attributeNames ::
      Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAccountAttributes' with the minimum fields required to make a request.
--
-- * 'attributeNames' - A list of attribute names.
mkDescribeAccountAttributes ::
  DescribeAccountAttributes
mkDescribeAccountAttributes =
  DescribeAccountAttributes' {attributeNames = Lude.Nothing}

-- | A list of attribute names.
--
-- /Note:/ Consider using 'attributeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daaAttributeNames :: Lens.Lens' DescribeAccountAttributes (Lude.Maybe [Lude.Text])
daaAttributeNames = Lens.lens (attributeNames :: DescribeAccountAttributes -> Lude.Maybe [Lude.Text]) (\s a -> s {attributeNames = a} :: DescribeAccountAttributes)
{-# DEPRECATED daaAttributeNames "Use generic-lens or generic-optics with 'attributeNames' instead." #-}

instance Lude.AWSRequest DescribeAccountAttributes where
  type
    Rs DescribeAccountAttributes =
      DescribeAccountAttributesResponse
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "DescribeAccountAttributesResult"
      ( \s h x ->
          DescribeAccountAttributesResponse'
            Lude.<$> ( x Lude..@? "AccountAttributes" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "AccountAttribute")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeAccountAttributes where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeAccountAttributes where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeAccountAttributes where
  toQuery DescribeAccountAttributes' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeAccountAttributes" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "AttributeNames"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "AttributeName" Lude.<$> attributeNames)
      ]

-- | /See:/ 'mkDescribeAccountAttributesResponse' smart constructor.
data DescribeAccountAttributesResponse = DescribeAccountAttributesResponse'
  { accountAttributes ::
      Lude.Maybe
        [AccountAttribute],
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

-- | Creates a value of 'DescribeAccountAttributesResponse' with the minimum fields required to make a request.
--
-- * 'accountAttributes' - A list of attributes assigned to an account.
-- * 'responseStatus' - The response status code.
mkDescribeAccountAttributesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeAccountAttributesResponse
mkDescribeAccountAttributesResponse pResponseStatus_ =
  DescribeAccountAttributesResponse'
    { accountAttributes =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of attributes assigned to an account.
--
-- /Note:/ Consider using 'accountAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daarsAccountAttributes :: Lens.Lens' DescribeAccountAttributesResponse (Lude.Maybe [AccountAttribute])
daarsAccountAttributes = Lens.lens (accountAttributes :: DescribeAccountAttributesResponse -> Lude.Maybe [AccountAttribute]) (\s a -> s {accountAttributes = a} :: DescribeAccountAttributesResponse)
{-# DEPRECATED daarsAccountAttributes "Use generic-lens or generic-optics with 'accountAttributes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daarsResponseStatus :: Lens.Lens' DescribeAccountAttributesResponse Lude.Int
daarsResponseStatus = Lens.lens (responseStatus :: DescribeAccountAttributesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeAccountAttributesResponse)
{-# DEPRECATED daarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
