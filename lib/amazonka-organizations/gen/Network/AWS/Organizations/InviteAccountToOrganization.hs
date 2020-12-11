{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    iatoNotes,
    iatoTags,
    iatoTarget,

    -- * Destructuring the response
    InviteAccountToOrganizationResponse (..),
    mkInviteAccountToOrganizationResponse,

    -- ** Response lenses
    iatorsHandshake,
    iatorsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkInviteAccountToOrganization' smart constructor.
data InviteAccountToOrganization = InviteAccountToOrganization'
  { notes ::
      Lude.Maybe
        (Lude.Sensitive Lude.Text),
    tags :: Lude.Maybe [Tag],
    target :: HandshakeParty
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InviteAccountToOrganization' with the minimum fields required to make a request.
--
-- * 'notes' - Additional information that you want to include in the generated email to the recipient account owner.
-- * 'tags' - A list of tags that you want to attach to the account when it becomes a member of the organization. For each tag in the list, you must specify both a tag key and a value. You can set the value to an empty string, but you can't set it to @null@ . For more information about tagging, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_tagging.html Tagging AWS Organizations resources> in the AWS Organizations User Guide.
--
-- /Important:/ Any tags in the request are checked for compliance with any applicable tag policies when the request is made. The request is rejected if the tags in the request don't match the requirements of the policy at that time. Tag policy compliance is /__not__ / checked again when the invitation is accepted and the tags are actually attached to the account. That means that if the tag policy changes between the invitation and the acceptance, then that tags could potentially be non-compliant.
-- * 'target' - The identifier (ID) of the AWS account that you want to invite to join your organization. This is a JSON object that contains the following elements:
--
-- @{ "Type": "ACCOUNT", "Id": "</__account id number__ / >" }@
-- If you use the AWS CLI, you can submit this as a single string, similar to the following example:
-- @--target Id=123456789012,Type=ACCOUNT@
-- If you specify @"Type": "ACCOUNT"@ , you must provide the AWS account ID number as the @Id@ . If you specify @"Type": "EMAIL"@ , you must specify the email address that is associated with the account.
-- @--target Id=diego@example.com,Type=EMAIL@
mkInviteAccountToOrganization ::
  -- | 'target'
  HandshakeParty ->
  InviteAccountToOrganization
mkInviteAccountToOrganization pTarget_ =
  InviteAccountToOrganization'
    { notes = Lude.Nothing,
      tags = Lude.Nothing,
      target = pTarget_
    }

-- | Additional information that you want to include in the generated email to the recipient account owner.
--
-- /Note:/ Consider using 'notes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iatoNotes :: Lens.Lens' InviteAccountToOrganization (Lude.Maybe (Lude.Sensitive Lude.Text))
iatoNotes = Lens.lens (notes :: InviteAccountToOrganization -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {notes = a} :: InviteAccountToOrganization)
{-# DEPRECATED iatoNotes "Use generic-lens or generic-optics with 'notes' instead." #-}

-- | A list of tags that you want to attach to the account when it becomes a member of the organization. For each tag in the list, you must specify both a tag key and a value. You can set the value to an empty string, but you can't set it to @null@ . For more information about tagging, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_tagging.html Tagging AWS Organizations resources> in the AWS Organizations User Guide.
--
-- /Important:/ Any tags in the request are checked for compliance with any applicable tag policies when the request is made. The request is rejected if the tags in the request don't match the requirements of the policy at that time. Tag policy compliance is /__not__ / checked again when the invitation is accepted and the tags are actually attached to the account. That means that if the tag policy changes between the invitation and the acceptance, then that tags could potentially be non-compliant.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iatoTags :: Lens.Lens' InviteAccountToOrganization (Lude.Maybe [Tag])
iatoTags = Lens.lens (tags :: InviteAccountToOrganization -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: InviteAccountToOrganization)
{-# DEPRECATED iatoTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The identifier (ID) of the AWS account that you want to invite to join your organization. This is a JSON object that contains the following elements:
--
-- @{ "Type": "ACCOUNT", "Id": "</__account id number__ / >" }@
-- If you use the AWS CLI, you can submit this as a single string, similar to the following example:
-- @--target Id=123456789012,Type=ACCOUNT@
-- If you specify @"Type": "ACCOUNT"@ , you must provide the AWS account ID number as the @Id@ . If you specify @"Type": "EMAIL"@ , you must specify the email address that is associated with the account.
-- @--target Id=diego@example.com,Type=EMAIL@
--
-- /Note:/ Consider using 'target' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iatoTarget :: Lens.Lens' InviteAccountToOrganization HandshakeParty
iatoTarget = Lens.lens (target :: InviteAccountToOrganization -> HandshakeParty) (\s a -> s {target = a} :: InviteAccountToOrganization)
{-# DEPRECATED iatoTarget "Use generic-lens or generic-optics with 'target' instead." #-}

instance Lude.AWSRequest InviteAccountToOrganization where
  type
    Rs InviteAccountToOrganization =
      InviteAccountToOrganizationResponse
  request = Req.postJSON organizationsService
  response =
    Res.receiveJSON
      ( \s h x ->
          InviteAccountToOrganizationResponse'
            Lude.<$> (x Lude..?> "Handshake") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders InviteAccountToOrganization where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSOrganizationsV20161128.InviteAccountToOrganization" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON InviteAccountToOrganization where
  toJSON InviteAccountToOrganization' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Notes" Lude..=) Lude.<$> notes,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("Target" Lude..= target)
          ]
      )

instance Lude.ToPath InviteAccountToOrganization where
  toPath = Lude.const "/"

instance Lude.ToQuery InviteAccountToOrganization where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkInviteAccountToOrganizationResponse' smart constructor.
data InviteAccountToOrganizationResponse = InviteAccountToOrganizationResponse'
  { handshake ::
      Lude.Maybe
        Handshake,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InviteAccountToOrganizationResponse' with the minimum fields required to make a request.
--
-- * 'handshake' - A structure that contains details about the handshake that is created to support this invitation request.
-- * 'responseStatus' - The response status code.
mkInviteAccountToOrganizationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  InviteAccountToOrganizationResponse
mkInviteAccountToOrganizationResponse pResponseStatus_ =
  InviteAccountToOrganizationResponse'
    { handshake = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A structure that contains details about the handshake that is created to support this invitation request.
--
-- /Note:/ Consider using 'handshake' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iatorsHandshake :: Lens.Lens' InviteAccountToOrganizationResponse (Lude.Maybe Handshake)
iatorsHandshake = Lens.lens (handshake :: InviteAccountToOrganizationResponse -> Lude.Maybe Handshake) (\s a -> s {handshake = a} :: InviteAccountToOrganizationResponse)
{-# DEPRECATED iatorsHandshake "Use generic-lens or generic-optics with 'handshake' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iatorsResponseStatus :: Lens.Lens' InviteAccountToOrganizationResponse Lude.Int
iatorsResponseStatus = Lens.lens (responseStatus :: InviteAccountToOrganizationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: InviteAccountToOrganizationResponse)
{-# DEPRECATED iatorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
