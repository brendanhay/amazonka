{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      InviteAccountToOrganization (..)
    , mkInviteAccountToOrganization
    -- ** Request lenses
    , iatoTarget
    , iatoNotes
    , iatoTags

    -- * Destructuring the response
    , InviteAccountToOrganizationResponse (..)
    , mkInviteAccountToOrganizationResponse
    -- ** Response lenses
    , iatorrsHandshake
    , iatorrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkInviteAccountToOrganization' smart constructor.
data InviteAccountToOrganization = InviteAccountToOrganization'
  { target :: Types.HandshakeParty
    -- ^ The identifier (ID) of the AWS account that you want to invite to join your organization. This is a JSON object that contains the following elements:
--
-- @{ "Type": "ACCOUNT", "Id": "</__account id number__ / >" }@ 
-- If you use the AWS CLI, you can submit this as a single string, similar to the following example:
-- @--target Id=123456789012,Type=ACCOUNT@ 
-- If you specify @"Type": "ACCOUNT"@ , you must provide the AWS account ID number as the @Id@ . If you specify @"Type": "EMAIL"@ , you must specify the email address that is associated with the account.
-- @--target Id=diego@example.com,Type=EMAIL@ 
  , notes :: Core.Maybe Types.HandshakeNotes
    -- ^ Additional information that you want to include in the generated email to the recipient account owner.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ A list of tags that you want to attach to the account when it becomes a member of the organization. For each tag in the list, you must specify both a tag key and a value. You can set the value to an empty string, but you can't set it to @null@ . For more information about tagging, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_tagging.html Tagging AWS Organizations resources> in the AWS Organizations User Guide.
--
-- /Important:/ Any tags in the request are checked for compliance with any applicable tag policies when the request is made. The request is rejected if the tags in the request don't match the requirements of the policy at that time. Tag policy compliance is /__not__ / checked again when the invitation is accepted and the tags are actually attached to the account. That means that if the tag policy changes between the invitation and the acceptance, then that tags could potentially be non-compliant.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InviteAccountToOrganization' value with any optional fields omitted.
mkInviteAccountToOrganization
    :: Types.HandshakeParty -- ^ 'target'
    -> InviteAccountToOrganization
mkInviteAccountToOrganization target
  = InviteAccountToOrganization'{target, notes = Core.Nothing,
                                 tags = Core.Nothing}

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
{-# INLINEABLE iatoTarget #-}
{-# DEPRECATED target "Use generic-lens or generic-optics with 'target' instead"  #-}

-- | Additional information that you want to include in the generated email to the recipient account owner.
--
-- /Note:/ Consider using 'notes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iatoNotes :: Lens.Lens' InviteAccountToOrganization (Core.Maybe Types.HandshakeNotes)
iatoNotes = Lens.field @"notes"
{-# INLINEABLE iatoNotes #-}
{-# DEPRECATED notes "Use generic-lens or generic-optics with 'notes' instead"  #-}

-- | A list of tags that you want to attach to the account when it becomes a member of the organization. For each tag in the list, you must specify both a tag key and a value. You can set the value to an empty string, but you can't set it to @null@ . For more information about tagging, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_tagging.html Tagging AWS Organizations resources> in the AWS Organizations User Guide.
--
-- /Important:/ Any tags in the request are checked for compliance with any applicable tag policies when the request is made. The request is rejected if the tags in the request don't match the requirements of the policy at that time. Tag policy compliance is /__not__ / checked again when the invitation is accepted and the tags are actually attached to the account. That means that if the tag policy changes between the invitation and the acceptance, then that tags could potentially be non-compliant.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iatoTags :: Lens.Lens' InviteAccountToOrganization (Core.Maybe [Types.Tag])
iatoTags = Lens.field @"tags"
{-# INLINEABLE iatoTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery InviteAccountToOrganization where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders InviteAccountToOrganization where
        toHeaders InviteAccountToOrganization{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSOrganizationsV20161128.InviteAccountToOrganization")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON InviteAccountToOrganization where
        toJSON InviteAccountToOrganization{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Target" Core..= target),
                  ("Notes" Core..=) Core.<$> notes, ("Tags" Core..=) Core.<$> tags])

instance Core.AWSRequest InviteAccountToOrganization where
        type Rs InviteAccountToOrganization =
             InviteAccountToOrganizationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 InviteAccountToOrganizationResponse' Core.<$>
                   (x Core..:? "Handshake") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkInviteAccountToOrganizationResponse' smart constructor.
data InviteAccountToOrganizationResponse = InviteAccountToOrganizationResponse'
  { handshake :: Core.Maybe Types.Handshake
    -- ^ A structure that contains details about the handshake that is created to support this invitation request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'InviteAccountToOrganizationResponse' value with any optional fields omitted.
mkInviteAccountToOrganizationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> InviteAccountToOrganizationResponse
mkInviteAccountToOrganizationResponse responseStatus
  = InviteAccountToOrganizationResponse'{handshake = Core.Nothing,
                                         responseStatus}

-- | A structure that contains details about the handshake that is created to support this invitation request.
--
-- /Note:/ Consider using 'handshake' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iatorrsHandshake :: Lens.Lens' InviteAccountToOrganizationResponse (Core.Maybe Types.Handshake)
iatorrsHandshake = Lens.field @"handshake"
{-# INLINEABLE iatorrsHandshake #-}
{-# DEPRECATED handshake "Use generic-lens or generic-optics with 'handshake' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iatorrsResponseStatus :: Lens.Lens' InviteAccountToOrganizationResponse Core.Int
iatorrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE iatorrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
