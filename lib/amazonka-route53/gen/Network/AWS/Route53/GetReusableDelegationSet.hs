{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.GetReusableDelegationSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a specified reusable delegation set, including the four name servers that are assigned to the delegation set.
module Network.AWS.Route53.GetReusableDelegationSet
  ( -- * Creating a request
    GetReusableDelegationSet (..),
    mkGetReusableDelegationSet,

    -- ** Request lenses
    grdsId,

    -- * Destructuring the response
    GetReusableDelegationSetResponse (..),
    mkGetReusableDelegationSetResponse,

    -- ** Response lenses
    grdsrsResponseStatus,
    grdsrsDelegationSet,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53.Types

-- | A request to get information about a specified reusable delegation set.
--
-- /See:/ 'mkGetReusableDelegationSet' smart constructor.
newtype GetReusableDelegationSet = GetReusableDelegationSet'
  { id ::
      ResourceId
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetReusableDelegationSet' with the minimum fields required to make a request.
--
-- * 'id' - The ID of the reusable delegation set that you want to get a list of name servers for.
mkGetReusableDelegationSet ::
  -- | 'id'
  ResourceId ->
  GetReusableDelegationSet
mkGetReusableDelegationSet pId_ =
  GetReusableDelegationSet' {id = pId_}

-- | The ID of the reusable delegation set that you want to get a list of name servers for.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdsId :: Lens.Lens' GetReusableDelegationSet ResourceId
grdsId = Lens.lens (id :: GetReusableDelegationSet -> ResourceId) (\s a -> s {id = a} :: GetReusableDelegationSet)
{-# DEPRECATED grdsId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest GetReusableDelegationSet where
  type Rs GetReusableDelegationSet = GetReusableDelegationSetResponse
  request = Req.get route53Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetReusableDelegationSetResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (x Lude..@ "DelegationSet")
      )

instance Lude.ToHeaders GetReusableDelegationSet where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetReusableDelegationSet where
  toPath GetReusableDelegationSet' {..} =
    Lude.mconcat ["/2013-04-01/delegationset/", Lude.toBS id]

instance Lude.ToQuery GetReusableDelegationSet where
  toQuery = Lude.const Lude.mempty

-- | A complex type that contains the response to the @GetReusableDelegationSet@ request.
--
-- /See:/ 'mkGetReusableDelegationSetResponse' smart constructor.
data GetReusableDelegationSetResponse = GetReusableDelegationSetResponse'
  { responseStatus ::
      Lude.Int,
    delegationSet ::
      DelegationSet
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetReusableDelegationSetResponse' with the minimum fields required to make a request.
--
-- * 'delegationSet' - A complex type that contains information about the reusable delegation set.
-- * 'responseStatus' - The response status code.
mkGetReusableDelegationSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'delegationSet'
  DelegationSet ->
  GetReusableDelegationSetResponse
mkGetReusableDelegationSetResponse pResponseStatus_ pDelegationSet_ =
  GetReusableDelegationSetResponse'
    { responseStatus =
        pResponseStatus_,
      delegationSet = pDelegationSet_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdsrsResponseStatus :: Lens.Lens' GetReusableDelegationSetResponse Lude.Int
grdsrsResponseStatus = Lens.lens (responseStatus :: GetReusableDelegationSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetReusableDelegationSetResponse)
{-# DEPRECATED grdsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A complex type that contains information about the reusable delegation set.
--
-- /Note:/ Consider using 'delegationSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdsrsDelegationSet :: Lens.Lens' GetReusableDelegationSetResponse DelegationSet
grdsrsDelegationSet = Lens.lens (delegationSet :: GetReusableDelegationSetResponse -> DelegationSet) (\s a -> s {delegationSet = a} :: GetReusableDelegationSetResponse)
{-# DEPRECATED grdsrsDelegationSet "Use generic-lens or generic-optics with 'delegationSet' instead." #-}
