{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    eafrsHandshake,
    eafrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkEnableAllFeatures' smart constructor.
data EnableAllFeatures = EnableAllFeatures'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnableAllFeatures' with the minimum fields required to make a request.
mkEnableAllFeatures ::
  EnableAllFeatures
mkEnableAllFeatures = EnableAllFeatures'

instance Lude.AWSRequest EnableAllFeatures where
  type Rs EnableAllFeatures = EnableAllFeaturesResponse
  request = Req.postJSON organizationsService
  response =
    Res.receiveJSON
      ( \s h x ->
          EnableAllFeaturesResponse'
            Lude.<$> (x Lude..?> "Handshake") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders EnableAllFeatures where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSOrganizationsV20161128.EnableAllFeatures" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON EnableAllFeatures where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath EnableAllFeatures where
  toPath = Lude.const "/"

instance Lude.ToQuery EnableAllFeatures where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkEnableAllFeaturesResponse' smart constructor.
data EnableAllFeaturesResponse = EnableAllFeaturesResponse'
  { handshake ::
      Lude.Maybe Handshake,
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnableAllFeaturesResponse' with the minimum fields required to make a request.
--
-- * 'handshake' - A structure that contains details about the handshake created to support this request to enable all features in the organization.
-- * 'responseStatus' - The response status code.
mkEnableAllFeaturesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  EnableAllFeaturesResponse
mkEnableAllFeaturesResponse pResponseStatus_ =
  EnableAllFeaturesResponse'
    { handshake = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A structure that contains details about the handshake created to support this request to enable all features in the organization.
--
-- /Note:/ Consider using 'handshake' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eafrsHandshake :: Lens.Lens' EnableAllFeaturesResponse (Lude.Maybe Handshake)
eafrsHandshake = Lens.lens (handshake :: EnableAllFeaturesResponse -> Lude.Maybe Handshake) (\s a -> s {handshake = a} :: EnableAllFeaturesResponse)
{-# DEPRECATED eafrsHandshake "Use generic-lens or generic-optics with 'handshake' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eafrsResponseStatus :: Lens.Lens' EnableAllFeaturesResponse Lude.Int
eafrsResponseStatus = Lens.lens (responseStatus :: EnableAllFeaturesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: EnableAllFeaturesResponse)
{-# DEPRECATED eafrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
