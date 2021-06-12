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
-- Module      : Network.AWS.Config.PutOrganizationConformancePack
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deploys conformance packs across member accounts in an AWS Organization.
--
-- Only a master account and a delegated administrator can call this API.
-- When calling this API with a delegated administrator, you must ensure
-- AWS Organizations @ListDelegatedAdministrator@ permissions are added.
--
-- This API enables organization service access for
-- @config-multiaccountsetup.amazonaws.com@ through the
-- @EnableAWSServiceAccess@ action and creates a service linked role
-- @AWSServiceRoleForConfigMultiAccountSetup@ in the master or delegated
-- administrator account of your organization. The service linked role is
-- created only when the role does not exist in the caller account. To use
-- this API with delegated administrator, register a delegated
-- administrator by calling AWS Organization @register-delegate-admin@ for
-- @config-multiaccountsetup.amazonaws.com@.
--
-- Prerequisite: Ensure you call @EnableAllFeatures@ API to enable all
-- features in an organization.
--
-- You must specify either the @TemplateS3Uri@ or the @TemplateBody@
-- parameter, but not both. If you provide both AWS Config uses the
-- @TemplateS3Uri@ parameter and ignores the @TemplateBody@ parameter.
--
-- AWS Config sets the state of a conformance pack to CREATE_IN_PROGRESS
-- and UPDATE_IN_PROGRESS until the conformance pack is created or updated.
-- You cannot update a conformance pack while it is in this state.
--
-- You can create 6 conformance packs with 25 AWS Config rules in each pack
-- and 3 delegated administrator per organization.
module Network.AWS.Config.PutOrganizationConformancePack
  ( -- * Creating a Request
    PutOrganizationConformancePack (..),
    newPutOrganizationConformancePack,

    -- * Request Lenses
    putOrganizationConformancePack_templateS3Uri,
    putOrganizationConformancePack_deliveryS3Bucket,
    putOrganizationConformancePack_deliveryS3KeyPrefix,
    putOrganizationConformancePack_excludedAccounts,
    putOrganizationConformancePack_templateBody,
    putOrganizationConformancePack_conformancePackInputParameters,
    putOrganizationConformancePack_organizationConformancePackName,

    -- * Destructuring the Response
    PutOrganizationConformancePackResponse (..),
    newPutOrganizationConformancePackResponse,

    -- * Response Lenses
    putOrganizationConformancePackResponse_organizationConformancePackArn,
    putOrganizationConformancePackResponse_httpStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutOrganizationConformancePack' smart constructor.
data PutOrganizationConformancePack = PutOrganizationConformancePack'
  { -- | Location of file containing the template body. The uri must point to the
    -- conformance pack template (max size: 300 KB).
    --
    -- You must have access to read Amazon S3 bucket.
    templateS3Uri :: Core.Maybe Core.Text,
    -- | Amazon S3 bucket where AWS Config stores conformance pack templates.
    --
    -- This field is optional.
    deliveryS3Bucket :: Core.Maybe Core.Text,
    -- | The prefix for the Amazon S3 bucket.
    --
    -- This field is optional.
    deliveryS3KeyPrefix :: Core.Maybe Core.Text,
    -- | A list of AWS accounts to be excluded from an organization conformance
    -- pack while deploying a conformance pack.
    excludedAccounts :: Core.Maybe [Core.Text],
    -- | A string containing full conformance pack template body. Structure
    -- containing the template body with a minimum length of 1 byte and a
    -- maximum length of 51,200 bytes.
    templateBody :: Core.Maybe Core.Text,
    -- | A list of @ConformancePackInputParameter@ objects.
    conformancePackInputParameters :: Core.Maybe [ConformancePackInputParameter],
    -- | Name of the organization conformance pack you want to create.
    organizationConformancePackName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutOrganizationConformancePack' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateS3Uri', 'putOrganizationConformancePack_templateS3Uri' - Location of file containing the template body. The uri must point to the
-- conformance pack template (max size: 300 KB).
--
-- You must have access to read Amazon S3 bucket.
--
-- 'deliveryS3Bucket', 'putOrganizationConformancePack_deliveryS3Bucket' - Amazon S3 bucket where AWS Config stores conformance pack templates.
--
-- This field is optional.
--
-- 'deliveryS3KeyPrefix', 'putOrganizationConformancePack_deliveryS3KeyPrefix' - The prefix for the Amazon S3 bucket.
--
-- This field is optional.
--
-- 'excludedAccounts', 'putOrganizationConformancePack_excludedAccounts' - A list of AWS accounts to be excluded from an organization conformance
-- pack while deploying a conformance pack.
--
-- 'templateBody', 'putOrganizationConformancePack_templateBody' - A string containing full conformance pack template body. Structure
-- containing the template body with a minimum length of 1 byte and a
-- maximum length of 51,200 bytes.
--
-- 'conformancePackInputParameters', 'putOrganizationConformancePack_conformancePackInputParameters' - A list of @ConformancePackInputParameter@ objects.
--
-- 'organizationConformancePackName', 'putOrganizationConformancePack_organizationConformancePackName' - Name of the organization conformance pack you want to create.
newPutOrganizationConformancePack ::
  -- | 'organizationConformancePackName'
  Core.Text ->
  PutOrganizationConformancePack
newPutOrganizationConformancePack
  pOrganizationConformancePackName_ =
    PutOrganizationConformancePack'
      { templateS3Uri =
          Core.Nothing,
        deliveryS3Bucket = Core.Nothing,
        deliveryS3KeyPrefix = Core.Nothing,
        excludedAccounts = Core.Nothing,
        templateBody = Core.Nothing,
        conformancePackInputParameters =
          Core.Nothing,
        organizationConformancePackName =
          pOrganizationConformancePackName_
      }

-- | Location of file containing the template body. The uri must point to the
-- conformance pack template (max size: 300 KB).
--
-- You must have access to read Amazon S3 bucket.
putOrganizationConformancePack_templateS3Uri :: Lens.Lens' PutOrganizationConformancePack (Core.Maybe Core.Text)
putOrganizationConformancePack_templateS3Uri = Lens.lens (\PutOrganizationConformancePack' {templateS3Uri} -> templateS3Uri) (\s@PutOrganizationConformancePack' {} a -> s {templateS3Uri = a} :: PutOrganizationConformancePack)

-- | Amazon S3 bucket where AWS Config stores conformance pack templates.
--
-- This field is optional.
putOrganizationConformancePack_deliveryS3Bucket :: Lens.Lens' PutOrganizationConformancePack (Core.Maybe Core.Text)
putOrganizationConformancePack_deliveryS3Bucket = Lens.lens (\PutOrganizationConformancePack' {deliveryS3Bucket} -> deliveryS3Bucket) (\s@PutOrganizationConformancePack' {} a -> s {deliveryS3Bucket = a} :: PutOrganizationConformancePack)

-- | The prefix for the Amazon S3 bucket.
--
-- This field is optional.
putOrganizationConformancePack_deliveryS3KeyPrefix :: Lens.Lens' PutOrganizationConformancePack (Core.Maybe Core.Text)
putOrganizationConformancePack_deliveryS3KeyPrefix = Lens.lens (\PutOrganizationConformancePack' {deliveryS3KeyPrefix} -> deliveryS3KeyPrefix) (\s@PutOrganizationConformancePack' {} a -> s {deliveryS3KeyPrefix = a} :: PutOrganizationConformancePack)

-- | A list of AWS accounts to be excluded from an organization conformance
-- pack while deploying a conformance pack.
putOrganizationConformancePack_excludedAccounts :: Lens.Lens' PutOrganizationConformancePack (Core.Maybe [Core.Text])
putOrganizationConformancePack_excludedAccounts = Lens.lens (\PutOrganizationConformancePack' {excludedAccounts} -> excludedAccounts) (\s@PutOrganizationConformancePack' {} a -> s {excludedAccounts = a} :: PutOrganizationConformancePack) Core.. Lens.mapping Lens._Coerce

-- | A string containing full conformance pack template body. Structure
-- containing the template body with a minimum length of 1 byte and a
-- maximum length of 51,200 bytes.
putOrganizationConformancePack_templateBody :: Lens.Lens' PutOrganizationConformancePack (Core.Maybe Core.Text)
putOrganizationConformancePack_templateBody = Lens.lens (\PutOrganizationConformancePack' {templateBody} -> templateBody) (\s@PutOrganizationConformancePack' {} a -> s {templateBody = a} :: PutOrganizationConformancePack)

-- | A list of @ConformancePackInputParameter@ objects.
putOrganizationConformancePack_conformancePackInputParameters :: Lens.Lens' PutOrganizationConformancePack (Core.Maybe [ConformancePackInputParameter])
putOrganizationConformancePack_conformancePackInputParameters = Lens.lens (\PutOrganizationConformancePack' {conformancePackInputParameters} -> conformancePackInputParameters) (\s@PutOrganizationConformancePack' {} a -> s {conformancePackInputParameters = a} :: PutOrganizationConformancePack) Core.. Lens.mapping Lens._Coerce

-- | Name of the organization conformance pack you want to create.
putOrganizationConformancePack_organizationConformancePackName :: Lens.Lens' PutOrganizationConformancePack Core.Text
putOrganizationConformancePack_organizationConformancePackName = Lens.lens (\PutOrganizationConformancePack' {organizationConformancePackName} -> organizationConformancePackName) (\s@PutOrganizationConformancePack' {} a -> s {organizationConformancePackName = a} :: PutOrganizationConformancePack)

instance
  Core.AWSRequest
    PutOrganizationConformancePack
  where
  type
    AWSResponse PutOrganizationConformancePack =
      PutOrganizationConformancePackResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutOrganizationConformancePackResponse'
            Core.<$> (x Core..?> "OrganizationConformancePackArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable PutOrganizationConformancePack

instance Core.NFData PutOrganizationConformancePack

instance
  Core.ToHeaders
    PutOrganizationConformancePack
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.PutOrganizationConformancePack" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PutOrganizationConformancePack where
  toJSON PutOrganizationConformancePack' {..} =
    Core.object
      ( Core.catMaybes
          [ ("TemplateS3Uri" Core..=) Core.<$> templateS3Uri,
            ("DeliveryS3Bucket" Core..=)
              Core.<$> deliveryS3Bucket,
            ("DeliveryS3KeyPrefix" Core..=)
              Core.<$> deliveryS3KeyPrefix,
            ("ExcludedAccounts" Core..=)
              Core.<$> excludedAccounts,
            ("TemplateBody" Core..=) Core.<$> templateBody,
            ("ConformancePackInputParameters" Core..=)
              Core.<$> conformancePackInputParameters,
            Core.Just
              ( "OrganizationConformancePackName"
                  Core..= organizationConformancePackName
              )
          ]
      )

instance Core.ToPath PutOrganizationConformancePack where
  toPath = Core.const "/"

instance Core.ToQuery PutOrganizationConformancePack where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutOrganizationConformancePackResponse' smart constructor.
data PutOrganizationConformancePackResponse = PutOrganizationConformancePackResponse'
  { -- | ARN of the organization conformance pack.
    organizationConformancePackArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutOrganizationConformancePackResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationConformancePackArn', 'putOrganizationConformancePackResponse_organizationConformancePackArn' - ARN of the organization conformance pack.
--
-- 'httpStatus', 'putOrganizationConformancePackResponse_httpStatus' - The response's http status code.
newPutOrganizationConformancePackResponse ::
  -- | 'httpStatus'
  Core.Int ->
  PutOrganizationConformancePackResponse
newPutOrganizationConformancePackResponse
  pHttpStatus_ =
    PutOrganizationConformancePackResponse'
      { organizationConformancePackArn =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | ARN of the organization conformance pack.
putOrganizationConformancePackResponse_organizationConformancePackArn :: Lens.Lens' PutOrganizationConformancePackResponse (Core.Maybe Core.Text)
putOrganizationConformancePackResponse_organizationConformancePackArn = Lens.lens (\PutOrganizationConformancePackResponse' {organizationConformancePackArn} -> organizationConformancePackArn) (\s@PutOrganizationConformancePackResponse' {} a -> s {organizationConformancePackArn = a} :: PutOrganizationConformancePackResponse)

-- | The response's http status code.
putOrganizationConformancePackResponse_httpStatus :: Lens.Lens' PutOrganizationConformancePackResponse Core.Int
putOrganizationConformancePackResponse_httpStatus = Lens.lens (\PutOrganizationConformancePackResponse' {httpStatus} -> httpStatus) (\s@PutOrganizationConformancePackResponse' {} a -> s {httpStatus = a} :: PutOrganizationConformancePackResponse)

instance
  Core.NFData
    PutOrganizationConformancePackResponse
