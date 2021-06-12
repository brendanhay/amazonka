{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.InviteAccountToOrganization
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends an invitation to another account to join your organization as a
-- member account. AWS Organizations sends email on your behalf to the
-- email address that is associated with the other account\'s owner. The
-- invitation is implemented as a Handshake whose details are in the
-- response.
--
-- -   You can invite AWS accounts only from the same seller as the
--     management account. For example, if your organization\'s management
--     account was created by Amazon Internet Services Pvt. Ltd (AISPL), an
--     AWS seller in India, you can invite only other AISPL accounts to
--     your organization. You can\'t combine accounts from AISPL and AWS or
--     from any other AWS seller. For more information, see
--     <http://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/useconsolidatedbilliing-India.html Consolidated Billing in India>.
--
-- -   If you receive an exception that indicates that you exceeded your
--     account limits for the organization or that the operation failed
--     because your organization is still initializing, wait one hour and
--     then try again. If the error persists after an hour, contact
--     <https://console.aws.amazon.com/support/home#/ AWS Support>.
--
-- If the request includes tags, then the requester must have the
-- @organizations:TagResource@ permission.
--
-- This operation can be called only from the organization\'s management
-- account.
module Network.AWS.Organizations.InviteAccountToOrganization
  ( -- * Creating a Request
    InviteAccountToOrganization (..),
    newInviteAccountToOrganization,

    -- * Request Lenses
    inviteAccountToOrganization_notes,
    inviteAccountToOrganization_tags,
    inviteAccountToOrganization_target,

    -- * Destructuring the Response
    InviteAccountToOrganizationResponse (..),
    newInviteAccountToOrganizationResponse,

    -- * Response Lenses
    inviteAccountToOrganizationResponse_handshake,
    inviteAccountToOrganizationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newInviteAccountToOrganization' smart constructor.
data InviteAccountToOrganization = InviteAccountToOrganization'
  { -- | Additional information that you want to include in the generated email
    -- to the recipient account owner.
    notes :: Core.Maybe (Core.Sensitive Core.Text),
    -- | A list of tags that you want to attach to the account when it becomes a
    -- member of the organization. For each tag in the list, you must specify
    -- both a tag key and a value. You can set the value to an empty string,
    -- but you can\'t set it to @null@. For more information about tagging, see
    -- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_tagging.html Tagging AWS Organizations resources>
    -- in the AWS Organizations User Guide.
    --
    -- Any tags in the request are checked for compliance with any applicable
    -- tag policies when the request is made. The request is rejected if the
    -- tags in the request don\'t match the requirements of the policy at that
    -- time. Tag policy compliance is /__not__/ checked again when the
    -- invitation is accepted and the tags are actually attached to the
    -- account. That means that if the tag policy changes between the
    -- invitation and the acceptance, then that tags could potentially be
    -- non-compliant.
    --
    -- If any one of the tags is invalid or if you exceed the allowed number of
    -- tags for an account, then the entire request fails and invitations are
    -- not sent.
    tags :: Core.Maybe [Tag],
    -- | The identifier (ID) of the AWS account that you want to invite to join
    -- your organization. This is a JSON object that contains the following
    -- elements:
    --
    -- @{ \"Type\": \"ACCOUNT\", \"Id\": \"\< account id number >\" }@
    --
    -- If you use the AWS CLI, you can submit this as a single string, similar
    -- to the following example:
    --
    -- @--target Id=123456789012,Type=ACCOUNT@
    --
    -- If you specify @\"Type\": \"ACCOUNT\"@, you must provide the AWS account
    -- ID number as the @Id@. If you specify @\"Type\": \"EMAIL\"@, you must
    -- specify the email address that is associated with the account.
    --
    -- @--target Id=diego\@example.com,Type=EMAIL@
    target :: HandshakeParty
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'InviteAccountToOrganization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notes', 'inviteAccountToOrganization_notes' - Additional information that you want to include in the generated email
-- to the recipient account owner.
--
-- 'tags', 'inviteAccountToOrganization_tags' - A list of tags that you want to attach to the account when it becomes a
-- member of the organization. For each tag in the list, you must specify
-- both a tag key and a value. You can set the value to an empty string,
-- but you can\'t set it to @null@. For more information about tagging, see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_tagging.html Tagging AWS Organizations resources>
-- in the AWS Organizations User Guide.
--
-- Any tags in the request are checked for compliance with any applicable
-- tag policies when the request is made. The request is rejected if the
-- tags in the request don\'t match the requirements of the policy at that
-- time. Tag policy compliance is /__not__/ checked again when the
-- invitation is accepted and the tags are actually attached to the
-- account. That means that if the tag policy changes between the
-- invitation and the acceptance, then that tags could potentially be
-- non-compliant.
--
-- If any one of the tags is invalid or if you exceed the allowed number of
-- tags for an account, then the entire request fails and invitations are
-- not sent.
--
-- 'target', 'inviteAccountToOrganization_target' - The identifier (ID) of the AWS account that you want to invite to join
-- your organization. This is a JSON object that contains the following
-- elements:
--
-- @{ \"Type\": \"ACCOUNT\", \"Id\": \"\< account id number >\" }@
--
-- If you use the AWS CLI, you can submit this as a single string, similar
-- to the following example:
--
-- @--target Id=123456789012,Type=ACCOUNT@
--
-- If you specify @\"Type\": \"ACCOUNT\"@, you must provide the AWS account
-- ID number as the @Id@. If you specify @\"Type\": \"EMAIL\"@, you must
-- specify the email address that is associated with the account.
--
-- @--target Id=diego\@example.com,Type=EMAIL@
newInviteAccountToOrganization ::
  -- | 'target'
  HandshakeParty ->
  InviteAccountToOrganization
newInviteAccountToOrganization pTarget_ =
  InviteAccountToOrganization'
    { notes = Core.Nothing,
      tags = Core.Nothing,
      target = pTarget_
    }

-- | Additional information that you want to include in the generated email
-- to the recipient account owner.
inviteAccountToOrganization_notes :: Lens.Lens' InviteAccountToOrganization (Core.Maybe Core.Text)
inviteAccountToOrganization_notes = Lens.lens (\InviteAccountToOrganization' {notes} -> notes) (\s@InviteAccountToOrganization' {} a -> s {notes = a} :: InviteAccountToOrganization) Core.. Lens.mapping Core._Sensitive

-- | A list of tags that you want to attach to the account when it becomes a
-- member of the organization. For each tag in the list, you must specify
-- both a tag key and a value. You can set the value to an empty string,
-- but you can\'t set it to @null@. For more information about tagging, see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_tagging.html Tagging AWS Organizations resources>
-- in the AWS Organizations User Guide.
--
-- Any tags in the request are checked for compliance with any applicable
-- tag policies when the request is made. The request is rejected if the
-- tags in the request don\'t match the requirements of the policy at that
-- time. Tag policy compliance is /__not__/ checked again when the
-- invitation is accepted and the tags are actually attached to the
-- account. That means that if the tag policy changes between the
-- invitation and the acceptance, then that tags could potentially be
-- non-compliant.
--
-- If any one of the tags is invalid or if you exceed the allowed number of
-- tags for an account, then the entire request fails and invitations are
-- not sent.
inviteAccountToOrganization_tags :: Lens.Lens' InviteAccountToOrganization (Core.Maybe [Tag])
inviteAccountToOrganization_tags = Lens.lens (\InviteAccountToOrganization' {tags} -> tags) (\s@InviteAccountToOrganization' {} a -> s {tags = a} :: InviteAccountToOrganization) Core.. Lens.mapping Lens._Coerce

-- | The identifier (ID) of the AWS account that you want to invite to join
-- your organization. This is a JSON object that contains the following
-- elements:
--
-- @{ \"Type\": \"ACCOUNT\", \"Id\": \"\< account id number >\" }@
--
-- If you use the AWS CLI, you can submit this as a single string, similar
-- to the following example:
--
-- @--target Id=123456789012,Type=ACCOUNT@
--
-- If you specify @\"Type\": \"ACCOUNT\"@, you must provide the AWS account
-- ID number as the @Id@. If you specify @\"Type\": \"EMAIL\"@, you must
-- specify the email address that is associated with the account.
--
-- @--target Id=diego\@example.com,Type=EMAIL@
inviteAccountToOrganization_target :: Lens.Lens' InviteAccountToOrganization HandshakeParty
inviteAccountToOrganization_target = Lens.lens (\InviteAccountToOrganization' {target} -> target) (\s@InviteAccountToOrganization' {} a -> s {target = a} :: InviteAccountToOrganization)

instance Core.AWSRequest InviteAccountToOrganization where
  type
    AWSResponse InviteAccountToOrganization =
      InviteAccountToOrganizationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          InviteAccountToOrganizationResponse'
            Core.<$> (x Core..?> "Handshake")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable InviteAccountToOrganization

instance Core.NFData InviteAccountToOrganization

instance Core.ToHeaders InviteAccountToOrganization where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSOrganizationsV20161128.InviteAccountToOrganization" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON InviteAccountToOrganization where
  toJSON InviteAccountToOrganization' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Notes" Core..=) Core.<$> notes,
            ("Tags" Core..=) Core.<$> tags,
            Core.Just ("Target" Core..= target)
          ]
      )

instance Core.ToPath InviteAccountToOrganization where
  toPath = Core.const "/"

instance Core.ToQuery InviteAccountToOrganization where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newInviteAccountToOrganizationResponse' smart constructor.
data InviteAccountToOrganizationResponse = InviteAccountToOrganizationResponse'
  { -- | A structure that contains details about the handshake that is created to
    -- support this invitation request.
    handshake :: Core.Maybe Handshake,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'InviteAccountToOrganizationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'handshake', 'inviteAccountToOrganizationResponse_handshake' - A structure that contains details about the handshake that is created to
-- support this invitation request.
--
-- 'httpStatus', 'inviteAccountToOrganizationResponse_httpStatus' - The response's http status code.
newInviteAccountToOrganizationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  InviteAccountToOrganizationResponse
newInviteAccountToOrganizationResponse pHttpStatus_ =
  InviteAccountToOrganizationResponse'
    { handshake =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A structure that contains details about the handshake that is created to
-- support this invitation request.
inviteAccountToOrganizationResponse_handshake :: Lens.Lens' InviteAccountToOrganizationResponse (Core.Maybe Handshake)
inviteAccountToOrganizationResponse_handshake = Lens.lens (\InviteAccountToOrganizationResponse' {handshake} -> handshake) (\s@InviteAccountToOrganizationResponse' {} a -> s {handshake = a} :: InviteAccountToOrganizationResponse)

-- | The response's http status code.
inviteAccountToOrganizationResponse_httpStatus :: Lens.Lens' InviteAccountToOrganizationResponse Core.Int
inviteAccountToOrganizationResponse_httpStatus = Lens.lens (\InviteAccountToOrganizationResponse' {httpStatus} -> httpStatus) (\s@InviteAccountToOrganizationResponse' {} a -> s {httpStatus = a} :: InviteAccountToOrganizationResponse)

instance
  Core.NFData
    InviteAccountToOrganizationResponse
