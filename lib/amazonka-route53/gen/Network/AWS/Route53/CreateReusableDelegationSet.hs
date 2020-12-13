{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.CreateReusableDelegationSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a delegation set (a group of four name servers) that can be reused by multiple hosted zones that were created by the same AWS account.
--
-- You can also create a reusable delegation set that uses the four name servers that are associated with an existing hosted zone. Specify the hosted zone ID in the @CreateReusableDelegationSet@ request.
-- For information about using a reusable delegation set to configure white label name servers, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/white-label-name-servers.html Configuring White Label Name Servers> .
-- The process for migrating existing hosted zones to use a reusable delegation set is comparable to the process for configuring white label name servers. You need to perform the following steps:
--
--     * Create a reusable delegation set.
--
--
--     * Recreate hosted zones, and reduce the TTL to 60 seconds or less.
--
--
--     * Recreate resource record sets in the new hosted zones.
--
--
--     * Change the registrar's name servers to use the name servers for the new hosted zones.
--
--
--     * Monitor traffic for the website or application.
--
--
--     * Change TTLs back to their original values.
--
--
-- If you want to migrate existing hosted zones to use a reusable delegation set, the existing hosted zones can't use any of the name servers that are assigned to the reusable delegation set. If one or more hosted zones do use one or more name servers that are assigned to the reusable delegation set, you can do one of the following:
--
--     * For small numbers of hosted zones—up to a few hundred—it's relatively easy to create reusable delegation sets until you get one that has four name servers that don't overlap with any of the name servers in your hosted zones.
--
--
--     * For larger numbers of hosted zones, the easiest solution is to use more than one reusable delegation set.
--
--
--     * For larger numbers of hosted zones, you can also migrate hosted zones that have overlapping name servers to hosted zones that don't have overlapping name servers, then migrate the hosted zones again to use the reusable delegation set.
module Network.AWS.Route53.CreateReusableDelegationSet
  ( -- * Creating a request
    CreateReusableDelegationSet (..),
    mkCreateReusableDelegationSet,

    -- ** Request lenses
    crdsHostedZoneId,
    crdsCallerReference,

    -- * Destructuring the response
    CreateReusableDelegationSetResponse (..),
    mkCreateReusableDelegationSetResponse,

    -- ** Response lenses
    crdsrsLocation,
    crdsrsDelegationSet,
    crdsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53.Types

-- | /See:/ 'mkCreateReusableDelegationSet' smart constructor.
data CreateReusableDelegationSet = CreateReusableDelegationSet'
  { -- | If you want to mark the delegation set for an existing hosted zone as reusable, the ID for that hosted zone.
    hostedZoneId :: Lude.Maybe ResourceId,
    -- | A unique string that identifies the request, and that allows you to retry failed @CreateReusableDelegationSet@ requests without the risk of executing the operation twice. You must use a unique @CallerReference@ string every time you submit a @CreateReusableDelegationSet@ request. @CallerReference@ can be any unique string, for example a date/time stamp.
    callerReference :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateReusableDelegationSet' with the minimum fields required to make a request.
--
-- * 'hostedZoneId' - If you want to mark the delegation set for an existing hosted zone as reusable, the ID for that hosted zone.
-- * 'callerReference' - A unique string that identifies the request, and that allows you to retry failed @CreateReusableDelegationSet@ requests without the risk of executing the operation twice. You must use a unique @CallerReference@ string every time you submit a @CreateReusableDelegationSet@ request. @CallerReference@ can be any unique string, for example a date/time stamp.
mkCreateReusableDelegationSet ::
  -- | 'callerReference'
  Lude.Text ->
  CreateReusableDelegationSet
mkCreateReusableDelegationSet pCallerReference_ =
  CreateReusableDelegationSet'
    { hostedZoneId = Lude.Nothing,
      callerReference = pCallerReference_
    }

-- | If you want to mark the delegation set for an existing hosted zone as reusable, the ID for that hosted zone.
--
-- /Note:/ Consider using 'hostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdsHostedZoneId :: Lens.Lens' CreateReusableDelegationSet (Lude.Maybe ResourceId)
crdsHostedZoneId = Lens.lens (hostedZoneId :: CreateReusableDelegationSet -> Lude.Maybe ResourceId) (\s a -> s {hostedZoneId = a} :: CreateReusableDelegationSet)
{-# DEPRECATED crdsHostedZoneId "Use generic-lens or generic-optics with 'hostedZoneId' instead." #-}

-- | A unique string that identifies the request, and that allows you to retry failed @CreateReusableDelegationSet@ requests without the risk of executing the operation twice. You must use a unique @CallerReference@ string every time you submit a @CreateReusableDelegationSet@ request. @CallerReference@ can be any unique string, for example a date/time stamp.
--
-- /Note:/ Consider using 'callerReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdsCallerReference :: Lens.Lens' CreateReusableDelegationSet Lude.Text
crdsCallerReference = Lens.lens (callerReference :: CreateReusableDelegationSet -> Lude.Text) (\s a -> s {callerReference = a} :: CreateReusableDelegationSet)
{-# DEPRECATED crdsCallerReference "Use generic-lens or generic-optics with 'callerReference' instead." #-}

instance Lude.AWSRequest CreateReusableDelegationSet where
  type
    Rs CreateReusableDelegationSet =
      CreateReusableDelegationSetResponse
  request = Req.postXML route53Service
  response =
    Res.receiveXML
      ( \s h x ->
          CreateReusableDelegationSetResponse'
            Lude.<$> (h Lude..# "Location")
            Lude.<*> (x Lude..@ "DelegationSet")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToElement CreateReusableDelegationSet where
  toElement =
    Lude.mkElement
      "{https://route53.amazonaws.com/doc/2013-04-01/}CreateReusableDelegationSetRequest"

instance Lude.ToHeaders CreateReusableDelegationSet where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateReusableDelegationSet where
  toPath = Lude.const "/2013-04-01/delegationset"

instance Lude.ToQuery CreateReusableDelegationSet where
  toQuery = Lude.const Lude.mempty

instance Lude.ToXML CreateReusableDelegationSet where
  toXML CreateReusableDelegationSet' {..} =
    Lude.mconcat
      [ "HostedZoneId" Lude.@= hostedZoneId,
        "CallerReference" Lude.@= callerReference
      ]

-- | /See:/ 'mkCreateReusableDelegationSetResponse' smart constructor.
data CreateReusableDelegationSetResponse = CreateReusableDelegationSetResponse'
  { -- | The unique URL representing the new reusable delegation set.
    location :: Lude.Text,
    -- | A complex type that contains name server information.
    delegationSet :: DelegationSet,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateReusableDelegationSetResponse' with the minimum fields required to make a request.
--
-- * 'location' - The unique URL representing the new reusable delegation set.
-- * 'delegationSet' - A complex type that contains name server information.
-- * 'responseStatus' - The response status code.
mkCreateReusableDelegationSetResponse ::
  -- | 'location'
  Lude.Text ->
  -- | 'delegationSet'
  DelegationSet ->
  -- | 'responseStatus'
  Lude.Int ->
  CreateReusableDelegationSetResponse
mkCreateReusableDelegationSetResponse
  pLocation_
  pDelegationSet_
  pResponseStatus_ =
    CreateReusableDelegationSetResponse'
      { location = pLocation_,
        delegationSet = pDelegationSet_,
        responseStatus = pResponseStatus_
      }

-- | The unique URL representing the new reusable delegation set.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdsrsLocation :: Lens.Lens' CreateReusableDelegationSetResponse Lude.Text
crdsrsLocation = Lens.lens (location :: CreateReusableDelegationSetResponse -> Lude.Text) (\s a -> s {location = a} :: CreateReusableDelegationSetResponse)
{-# DEPRECATED crdsrsLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | A complex type that contains name server information.
--
-- /Note:/ Consider using 'delegationSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdsrsDelegationSet :: Lens.Lens' CreateReusableDelegationSetResponse DelegationSet
crdsrsDelegationSet = Lens.lens (delegationSet :: CreateReusableDelegationSetResponse -> DelegationSet) (\s a -> s {delegationSet = a} :: CreateReusableDelegationSetResponse)
{-# DEPRECATED crdsrsDelegationSet "Use generic-lens or generic-optics with 'delegationSet' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdsrsResponseStatus :: Lens.Lens' CreateReusableDelegationSetResponse Lude.Int
crdsrsResponseStatus = Lens.lens (responseStatus :: CreateReusableDelegationSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateReusableDelegationSetResponse)
{-# DEPRECATED crdsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
