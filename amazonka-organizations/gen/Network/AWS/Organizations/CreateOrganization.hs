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
-- Module      : Network.AWS.Organizations.CreateOrganization
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS organization. The account whose user is calling the
-- @CreateOrganization@ operation automatically becomes the
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_getting-started_concepts.html#account management account>
-- of the new organization.
--
-- This operation must be called using credentials from the account that is
-- to become the new organization\'s management account. The principal must
-- also have the relevant IAM permissions.
--
-- By default (or if you set the @FeatureSet@ parameter to @ALL@), the new
-- organization is created with all features enabled and service control
-- policies automatically enabled in the root. If you instead choose to
-- create the organization supporting only the consolidated billing
-- features by setting the @FeatureSet@ parameter to
-- @CONSOLIDATED_BILLING\"@, no policy types are enabled by default, and
-- you can\'t use organization policies
module Network.AWS.Organizations.CreateOrganization
  ( -- * Creating a Request
    CreateOrganization (..),
    newCreateOrganization,

    -- * Request Lenses
    createOrganization_featureSet,

    -- * Destructuring the Response
    CreateOrganizationResponse (..),
    newCreateOrganizationResponse,

    -- * Response Lenses
    createOrganizationResponse_organization,
    createOrganizationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateOrganization' smart constructor.
data CreateOrganization = CreateOrganization'
  { -- | Specifies the feature set supported by the new organization. Each
    -- feature set supports different levels of functionality.
    --
    -- -   @CONSOLIDATED_BILLING@: All member accounts have their bills
    --     consolidated to and paid by the management account. For more
    --     information, see
    --     <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_getting-started_concepts.html#feature-set-cb-only Consolidated billing>
    --     in the /AWS Organizations User Guide./
    --
    --     The consolidated billing feature subset isn\'t available for
    --     organizations in the AWS GovCloud (US) Region.
    --
    -- -   @ALL@: In addition to all the features supported by the consolidated
    --     billing feature set, the management account can also apply any
    --     policy type to any member account in the organization. For more
    --     information, see
    --     <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_getting-started_concepts.html#feature-set-all All features>
    --     in the /AWS Organizations User Guide./
    featureSet :: Prelude.Maybe OrganizationFeatureSet
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateOrganization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'featureSet', 'createOrganization_featureSet' - Specifies the feature set supported by the new organization. Each
-- feature set supports different levels of functionality.
--
-- -   @CONSOLIDATED_BILLING@: All member accounts have their bills
--     consolidated to and paid by the management account. For more
--     information, see
--     <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_getting-started_concepts.html#feature-set-cb-only Consolidated billing>
--     in the /AWS Organizations User Guide./
--
--     The consolidated billing feature subset isn\'t available for
--     organizations in the AWS GovCloud (US) Region.
--
-- -   @ALL@: In addition to all the features supported by the consolidated
--     billing feature set, the management account can also apply any
--     policy type to any member account in the organization. For more
--     information, see
--     <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_getting-started_concepts.html#feature-set-all All features>
--     in the /AWS Organizations User Guide./
newCreateOrganization ::
  CreateOrganization
newCreateOrganization =
  CreateOrganization' {featureSet = Prelude.Nothing}

-- | Specifies the feature set supported by the new organization. Each
-- feature set supports different levels of functionality.
--
-- -   @CONSOLIDATED_BILLING@: All member accounts have their bills
--     consolidated to and paid by the management account. For more
--     information, see
--     <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_getting-started_concepts.html#feature-set-cb-only Consolidated billing>
--     in the /AWS Organizations User Guide./
--
--     The consolidated billing feature subset isn\'t available for
--     organizations in the AWS GovCloud (US) Region.
--
-- -   @ALL@: In addition to all the features supported by the consolidated
--     billing feature set, the management account can also apply any
--     policy type to any member account in the organization. For more
--     information, see
--     <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_getting-started_concepts.html#feature-set-all All features>
--     in the /AWS Organizations User Guide./
createOrganization_featureSet :: Lens.Lens' CreateOrganization (Prelude.Maybe OrganizationFeatureSet)
createOrganization_featureSet = Lens.lens (\CreateOrganization' {featureSet} -> featureSet) (\s@CreateOrganization' {} a -> s {featureSet = a} :: CreateOrganization)

instance Core.AWSRequest CreateOrganization where
  type
    AWSResponse CreateOrganization =
      CreateOrganizationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateOrganizationResponse'
            Prelude.<$> (x Core..?> "Organization")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateOrganization

instance Prelude.NFData CreateOrganization

instance Core.ToHeaders CreateOrganization where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSOrganizationsV20161128.CreateOrganization" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateOrganization where
  toJSON CreateOrganization' {..} =
    Core.object
      ( Prelude.catMaybes
          [("FeatureSet" Core..=) Prelude.<$> featureSet]
      )

instance Core.ToPath CreateOrganization where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateOrganization where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateOrganizationResponse' smart constructor.
data CreateOrganizationResponse = CreateOrganizationResponse'
  { -- | A structure that contains details about the newly created organization.
    organization :: Prelude.Maybe Organization,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateOrganizationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organization', 'createOrganizationResponse_organization' - A structure that contains details about the newly created organization.
--
-- 'httpStatus', 'createOrganizationResponse_httpStatus' - The response's http status code.
newCreateOrganizationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateOrganizationResponse
newCreateOrganizationResponse pHttpStatus_ =
  CreateOrganizationResponse'
    { organization =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A structure that contains details about the newly created organization.
createOrganizationResponse_organization :: Lens.Lens' CreateOrganizationResponse (Prelude.Maybe Organization)
createOrganizationResponse_organization = Lens.lens (\CreateOrganizationResponse' {organization} -> organization) (\s@CreateOrganizationResponse' {} a -> s {organization = a} :: CreateOrganizationResponse)

-- | The response's http status code.
createOrganizationResponse_httpStatus :: Lens.Lens' CreateOrganizationResponse Prelude.Int
createOrganizationResponse_httpStatus = Lens.lens (\CreateOrganizationResponse' {httpStatus} -> httpStatus) (\s@CreateOrganizationResponse' {} a -> s {httpStatus = a} :: CreateOrganizationResponse)

instance Prelude.NFData CreateOrganizationResponse
