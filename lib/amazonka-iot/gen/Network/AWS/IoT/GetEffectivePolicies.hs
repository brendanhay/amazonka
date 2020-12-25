{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.GetEffectivePolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the policies that have an effect on the authorization behavior of the specified device when it connects to the AWS IoT device gateway.
module Network.AWS.IoT.GetEffectivePolicies
  ( -- * Creating a request
    GetEffectivePolicies (..),
    mkGetEffectivePolicies,

    -- ** Request lenses
    gepCognitoIdentityPoolId,
    gepPrincipal,
    gepThingName,

    -- * Destructuring the response
    GetEffectivePoliciesResponse (..),
    mkGetEffectivePoliciesResponse,

    -- ** Response lenses
    geprrsEffectivePolicies,
    geprrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetEffectivePolicies' smart constructor.
data GetEffectivePolicies = GetEffectivePolicies'
  { -- | The Cognito identity pool ID.
    cognitoIdentityPoolId :: Core.Maybe Types.CognitoIdentityPoolId,
    -- | The principal. Valid principals are CertificateArn (arn:aws:iot:/region/ :/accountId/ :cert//certificateId/ ), thingGroupArn (arn:aws:iot:/region/ :/accountId/ :thinggroup//groupName/ ) and CognitoId (/region/ :/id/ ).
    principal :: Core.Maybe Types.Principal,
    -- | The thing name.
    thingName :: Core.Maybe Types.ThingName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetEffectivePolicies' value with any optional fields omitted.
mkGetEffectivePolicies ::
  GetEffectivePolicies
mkGetEffectivePolicies =
  GetEffectivePolicies'
    { cognitoIdentityPoolId = Core.Nothing,
      principal = Core.Nothing,
      thingName = Core.Nothing
    }

-- | The Cognito identity pool ID.
--
-- /Note:/ Consider using 'cognitoIdentityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gepCognitoIdentityPoolId :: Lens.Lens' GetEffectivePolicies (Core.Maybe Types.CognitoIdentityPoolId)
gepCognitoIdentityPoolId = Lens.field @"cognitoIdentityPoolId"
{-# DEPRECATED gepCognitoIdentityPoolId "Use generic-lens or generic-optics with 'cognitoIdentityPoolId' instead." #-}

-- | The principal. Valid principals are CertificateArn (arn:aws:iot:/region/ :/accountId/ :cert//certificateId/ ), thingGroupArn (arn:aws:iot:/region/ :/accountId/ :thinggroup//groupName/ ) and CognitoId (/region/ :/id/ ).
--
-- /Note:/ Consider using 'principal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gepPrincipal :: Lens.Lens' GetEffectivePolicies (Core.Maybe Types.Principal)
gepPrincipal = Lens.field @"principal"
{-# DEPRECATED gepPrincipal "Use generic-lens or generic-optics with 'principal' instead." #-}

-- | The thing name.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gepThingName :: Lens.Lens' GetEffectivePolicies (Core.Maybe Types.ThingName)
gepThingName = Lens.field @"thingName"
{-# DEPRECATED gepThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

instance Core.FromJSON GetEffectivePolicies where
  toJSON GetEffectivePolicies {..} =
    Core.object
      ( Core.catMaybes
          [ ("cognitoIdentityPoolId" Core..=) Core.<$> cognitoIdentityPoolId,
            ("principal" Core..=) Core.<$> principal
          ]
      )

instance Core.AWSRequest GetEffectivePolicies where
  type Rs GetEffectivePolicies = GetEffectivePoliciesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/effective-policies",
        Core._rqQuery = Core.toQueryValue "thingName" Core.<$> thingName,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetEffectivePoliciesResponse'
            Core.<$> (x Core..:? "effectivePolicies")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetEffectivePoliciesResponse' smart constructor.
data GetEffectivePoliciesResponse = GetEffectivePoliciesResponse'
  { -- | The effective policies.
    effectivePolicies :: Core.Maybe [Types.EffectivePolicy],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetEffectivePoliciesResponse' value with any optional fields omitted.
mkGetEffectivePoliciesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetEffectivePoliciesResponse
mkGetEffectivePoliciesResponse responseStatus =
  GetEffectivePoliciesResponse'
    { effectivePolicies = Core.Nothing,
      responseStatus
    }

-- | The effective policies.
--
-- /Note:/ Consider using 'effectivePolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
geprrsEffectivePolicies :: Lens.Lens' GetEffectivePoliciesResponse (Core.Maybe [Types.EffectivePolicy])
geprrsEffectivePolicies = Lens.field @"effectivePolicies"
{-# DEPRECATED geprrsEffectivePolicies "Use generic-lens or generic-optics with 'effectivePolicies' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
geprrsResponseStatus :: Lens.Lens' GetEffectivePoliciesResponse Core.Int
geprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED geprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
