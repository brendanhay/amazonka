{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DescribeAccountAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all of the attributes for a customer account. The attributes include Amazon RDS quotas for the account, such as the number of DB instances allowed. The description for a quota includes the quota name, current usage toward that quota, and the quota's maximum value.
--
-- This command doesn't take any parameters.
module Network.AWS.RDS.DescribeAccountAttributes
  ( -- * Creating a request
    DescribeAccountAttributes (..),
    mkDescribeAccountAttributes,

    -- * Destructuring the response
    DescribeAccountAttributesResponse (..),
    mkDescribeAccountAttributesResponse,

    -- ** Response lenses
    daarsAccountQuotas,
    daarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDescribeAccountAttributes' smart constructor.
data DescribeAccountAttributes = DescribeAccountAttributes'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAccountAttributes' with the minimum fields required to make a request.
mkDescribeAccountAttributes ::
  DescribeAccountAttributes
mkDescribeAccountAttributes = DescribeAccountAttributes'

instance Lude.AWSRequest DescribeAccountAttributes where
  type
    Rs DescribeAccountAttributes =
      DescribeAccountAttributesResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "DescribeAccountAttributesResult"
      ( \s h x ->
          DescribeAccountAttributesResponse'
            Lude.<$> ( x Lude..@? "AccountQuotas" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "AccountQuota")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeAccountAttributes where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeAccountAttributes where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeAccountAttributes where
  toQuery =
    Lude.const
      ( Lude.mconcat
          [ "Action" Lude.=: ("DescribeAccountAttributes" :: Lude.ByteString),
            "Version" Lude.=: ("2014-10-31" :: Lude.ByteString)
          ]
      )

-- | Data returned by the __DescribeAccountAttributes__ action.
--
-- /See:/ 'mkDescribeAccountAttributesResponse' smart constructor.
data DescribeAccountAttributesResponse = DescribeAccountAttributesResponse'
  { accountQuotas ::
      Lude.Maybe
        [AccountQuota],
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
-- * 'accountQuotas' - A list of @AccountQuota@ objects. Within this list, each quota has a name, a count of usage toward the quota maximum, and a maximum value for the quota.
-- * 'responseStatus' - The response status code.
mkDescribeAccountAttributesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeAccountAttributesResponse
mkDescribeAccountAttributesResponse pResponseStatus_ =
  DescribeAccountAttributesResponse'
    { accountQuotas = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of @AccountQuota@ objects. Within this list, each quota has a name, a count of usage toward the quota maximum, and a maximum value for the quota.
--
-- /Note:/ Consider using 'accountQuotas' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daarsAccountQuotas :: Lens.Lens' DescribeAccountAttributesResponse (Lude.Maybe [AccountQuota])
daarsAccountQuotas = Lens.lens (accountQuotas :: DescribeAccountAttributesResponse -> Lude.Maybe [AccountQuota]) (\s a -> s {accountQuotas = a} :: DescribeAccountAttributesResponse)
{-# DEPRECATED daarsAccountQuotas "Use generic-lens or generic-optics with 'accountQuotas' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daarsResponseStatus :: Lens.Lens' DescribeAccountAttributesResponse Lude.Int
daarsResponseStatus = Lens.lens (responseStatus :: DescribeAccountAttributesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeAccountAttributesResponse)
{-# DEPRECATED daarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
