{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.EnableAllFeatures
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables all features in an organization. This enables the use of organization policies that can restrict the services and actions that can be called in each account. Until you enable all features, you have access only to consolidated billing, and you can't use any of the advanced account administration features that AWS Organizations supports. For more information, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_org_support-all-features.html Enabling All Features in Your Organization> in the /AWS Organizations User Guide./
--
-- /Important:/ This operation is required only for organizations that were created explicitly with only the consolidated billing features enabled. Calling this operation sends a handshake to every invited account in the organization. The feature set change can be finalized and the additional features enabled only after all administrators in the invited accounts approve the change by accepting the handshake.
-- After you enable all features, you can separately enable or disable individual policy types in a root using 'EnablePolicyType' and 'DisablePolicyType' . To see the status of policy types in a root, use 'ListRoots' .
-- After all invited member accounts accept the handshake, you finalize the feature set change by accepting the handshake that contains @"Action": "ENABLE_ALL_FEATURES"@ . This completes the change.
-- After you enable all features in your organization, the management account in the organization can apply policies on all member accounts. These policies can restrict what users and even administrators in those accounts can do. The management account can apply policies that prevent accounts from leaving the organization. Ensure that your account administrators are aware of this.
-- This operation can be called only from the organization's management account.
module Network.AWS.Organizations.EnableAllFeatures
  ( -- * Creating a request
    EnableAllFeatures (..),
    mkEnableAllFeatures,

    -- * Destructuring the response
    EnableAllFeaturesResponse (..),
    mkEnableAllFeaturesResponse,

    -- ** Response lenses
    eafrrsHandshake,
    eafrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkEnableAllFeatures' smart constructor.
data EnableAllFeatures = EnableAllFeatures'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnableAllFeatures' value with any optional fields omitted.
mkEnableAllFeatures ::
  EnableAllFeatures
mkEnableAllFeatures = EnableAllFeatures'

instance Core.FromJSON EnableAllFeatures where
  toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest EnableAllFeatures where
  type Rs EnableAllFeatures = EnableAllFeaturesResponse
  request x@_ =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSOrganizationsV20161128.EnableAllFeatures")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          EnableAllFeaturesResponse'
            Core.<$> (x Core..:? "Handshake") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkEnableAllFeaturesResponse' smart constructor.
data EnableAllFeaturesResponse = EnableAllFeaturesResponse'
  { -- | A structure that contains details about the handshake created to support this request to enable all features in the organization.
    handshake :: Core.Maybe Types.Handshake,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'EnableAllFeaturesResponse' value with any optional fields omitted.
mkEnableAllFeaturesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  EnableAllFeaturesResponse
mkEnableAllFeaturesResponse responseStatus =
  EnableAllFeaturesResponse'
    { handshake = Core.Nothing,
      responseStatus
    }

-- | A structure that contains details about the handshake created to support this request to enable all features in the organization.
--
-- /Note:/ Consider using 'handshake' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eafrrsHandshake :: Lens.Lens' EnableAllFeaturesResponse (Core.Maybe Types.Handshake)
eafrrsHandshake = Lens.field @"handshake"
{-# DEPRECATED eafrrsHandshake "Use generic-lens or generic-optics with 'handshake' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eafrrsResponseStatus :: Lens.Lens' EnableAllFeaturesResponse Core.Int
eafrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED eafrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
