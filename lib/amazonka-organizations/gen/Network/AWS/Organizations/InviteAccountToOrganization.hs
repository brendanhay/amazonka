{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.InviteAccountToOrganization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends an invitation to another account to join your organization as a member account. AWS Organizations sends email on your behalf to the email address that is associated with the other account's owner. The invitation is implemented as a 'Handshake' whose details are in the response.
--
-- /Important:/
--     * You can invite AWS accounts only from the same seller as the management account. For example, if your organization's management account was created by Amazon Internet Services Pvt. Ltd (AISPL), an AWS seller in India, you can invite only other AISPL accounts to your organization. You can't combine accounts from AISPL and AWS or from any other AWS seller. For more information, see <http://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/useconsolidatedbilliing-India.html Consolidated Billing in India> .
--
--
--     * If you receive an exception that indicates that you exceeded your account limits for the organization or that the operation failed because your organization is still initializing, wait one hour and then try again. If the error persists after an hour, contact <https://console.aws.amazon.com/support/home#/ AWS Support> .
--
--
-- If the request includes tags, then the requester must have the @organizations:TagResource@ permission.
-- This operation can be called only from the organization's management account.
module Network.AWS.Organizations.InviteAccountToOrganization
  ( -- * Creating a request
    InviteAccountToOrganization (..),
    mkInviteAccountToOrganization,

    -- ** Request lenses
    iatoTarget,
    iatoNotes,
    iatoTags,

    -- * Destructuring the response
    InviteAccountToOrganizationResponse (..),
    mkInviteAccountToOrganizationResponse,

    -- ** Response lenses
    iatorrsHandshake,
    iatorrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkInviteAccountToOrganization' smart constructor.
data InviteAccountToOrganization = InviteAccountToOrganization'
  { -- | The identifier (ID) of the AWS account that you want to invite to join your organization. This is a JSON object that contains the following elements:
    --
    -- @{ "Type": "ACCOUNT", "Id": "</__account id number__ / >" }@
    -- If you use the AWS CLI, you can submit this as a single string, similar to the following example:
    -- @--target Id=123456789012,Type=ACCOUNT@
    -- If you specify @"Type": "ACCOUNT"@ , you must provide the AWS account ID number as the @Id@ . If you specify @"Type": "EMAIL"@ , you must specify the email address that is associated with the account.
    -- @--target Id=diego@example.com,Type=EMAIL@
    target :: Types.HandshakeParty,
    -- | Additional information that you want to include in the generated email to the recipient account owner.
    notes :: Core.Maybe Types.HandshakeNotes,
    -- | A list of tags that you want to attach to the account when it becomes a member of the organization. For each tag in the list, you must specify both a tag key and a value. You can set the value to an empty string, but you can't set it to @null@ . For more information about tagging, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_tagging.html Tagging AWS Organizations resources> in the AWS Organizations User Guide.
    --
    -- /Important:/ Any tags in the request are checked for compliance with any applicable tag policies when the request is made. The request is rejected if the tags in the request don't match the requirements of the policy at that time. Tag policy compliance is /__not__ / checked again when the invitation is accepted and the tags are actually attached to the account. That means that if the tag policy changes between the invitation and the acceptance, then that tags could potentially be non-compliant.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InviteAccountToOrganization' value with any optional fields omitted.
mkInviteAccountToOrganization ::
  -- | 'target'
  Types.HandshakeParty ->
  InviteAccountToOrganization
mkInviteAccountToOrganization target =
  InviteAccountToOrganization'
    { target,
      notes = Core.Nothing,
      tags = Core.Nothing
    }

-- | The identifier (ID) of the AWS account that you want to invite to join your organization. This is a JSON object that contains the following elements:
--
-- @{ "Type": "ACCOUNT", "Id": "</__account id number__ / >" }@
-- If you use the AWS CLI, you can submit this as a single string, similar to the following example:
-- @--target Id=123456789012,Type=ACCOUNT@
-- If you specify @"Type": "ACCOUNT"@ , you must provide the AWS account ID number as the @Id@ . If you specify @"Type": "EMAIL"@ , you must specify the email address that is associated with the account.
-- @--target Id=diego@example.com,Type=EMAIL@
--
-- /Note:/ Consider using 'target' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iatoTarget :: Lens.Lens' InviteAccountToOrganization Types.HandshakeParty
iatoTarget = Lens.field @"target"
{-# DEPRECATED iatoTarget "Use generic-lens or generic-optics with 'target' instead." #-}

-- | Additional information that you want to include in the generated email to the recipient account owner.
--
-- /Note:/ Consider using 'notes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iatoNotes :: Lens.Lens' InviteAccountToOrganization (Core.Maybe Types.HandshakeNotes)
iatoNotes = Lens.field @"notes"
{-# DEPRECATED iatoNotes "Use generic-lens or generic-optics with 'notes' instead." #-}

-- | A list of tags that you want to attach to the account when it becomes a member of the organization. For each tag in the list, you must specify both a tag key and a value. You can set the value to an empty string, but you can't set it to @null@ . For more information about tagging, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_tagging.html Tagging AWS Organizations resources> in the AWS Organizations User Guide.
--
-- /Important:/ Any tags in the request are checked for compliance with any applicable tag policies when the request is made. The request is rejected if the tags in the request don't match the requirements of the policy at that time. Tag policy compliance is /__not__ / checked again when the invitation is accepted and the tags are actually attached to the account. That means that if the tag policy changes between the invitation and the acceptance, then that tags could potentially be non-compliant.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iatoTags :: Lens.Lens' InviteAccountToOrganization (Core.Maybe [Types.Tag])
iatoTags = Lens.field @"tags"
{-# DEPRECATED iatoTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON InviteAccountToOrganization where
  toJSON InviteAccountToOrganization {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Target" Core..= target),
            ("Notes" Core..=) Core.<$> notes,
            ("Tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest InviteAccountToOrganization where
  type
    Rs InviteAccountToOrganization =
      InviteAccountToOrganizationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSOrganizationsV20161128.InviteAccountToOrganization"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          InviteAccountToOrganizationResponse'
            Core.<$> (x Core..:? "Handshake") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkInviteAccountToOrganizationResponse' smart constructor.
data InviteAccountToOrganizationResponse = InviteAccountToOrganizationResponse'
  { -- | A structure that contains details about the handshake that is created to support this invitation request.
    handshake :: Core.Maybe Types.Handshake,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'InviteAccountToOrganizationResponse' value with any optional fields omitted.
mkInviteAccountToOrganizationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  InviteAccountToOrganizationResponse
mkInviteAccountToOrganizationResponse responseStatus =
  InviteAccountToOrganizationResponse'
    { handshake = Core.Nothing,
      responseStatus
    }

-- | A structure that contains details about the handshake that is created to support this invitation request.
--
-- /Note:/ Consider using 'handshake' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iatorrsHandshake :: Lens.Lens' InviteAccountToOrganizationResponse (Core.Maybe Types.Handshake)
iatorrsHandshake = Lens.field @"handshake"
{-# DEPRECATED iatorrsHandshake "Use generic-lens or generic-optics with 'handshake' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iatorrsResponseStatus :: Lens.Lens' InviteAccountToOrganizationResponse Core.Int
iatorrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED iatorrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
