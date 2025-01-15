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
-- Module      : Amazonka.Config.PutOrganizationConformancePack
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deploys conformance packs across member accounts in an Amazon Web
-- Services Organization. For information on how many organization
-- conformance packs and how many Config rules you can have per account,
-- see
-- <https://docs.aws.amazon.com/config/latest/developerguide/configlimits.html Service Limits>
-- in the Config Developer Guide.
--
-- Only a management account and a delegated administrator can call this
-- API. When calling this API with a delegated administrator, you must
-- ensure Organizations @ListDelegatedAdministrator@ permissions are added.
-- An organization can have up to 3 delegated administrators.
--
-- This API enables organization service access for
-- @config-multiaccountsetup.amazonaws.com@ through the
-- @EnableAWSServiceAccess@ action and creates a service-linked role
-- @AWSServiceRoleForConfigMultiAccountSetup@ in the management or
-- delegated administrator account of your organization. The service-linked
-- role is created only when the role does not exist in the caller account.
-- To use this API with delegated administrator, register a delegated
-- administrator by calling Amazon Web Services Organization
-- @register-delegate-admin@ for @config-multiaccountsetup.amazonaws.com@.
--
-- Prerequisite: Ensure you call @EnableAllFeatures@ API to enable all
-- features in an organization.
--
-- You must specify either the @TemplateS3Uri@ or the @TemplateBody@
-- parameter, but not both. If you provide both Config uses the
-- @TemplateS3Uri@ parameter and ignores the @TemplateBody@ parameter.
--
-- Config sets the state of a conformance pack to CREATE_IN_PROGRESS and
-- UPDATE_IN_PROGRESS until the conformance pack is created or updated. You
-- cannot update a conformance pack while it is in this state.
module Amazonka.Config.PutOrganizationConformancePack
  ( -- * Creating a Request
    PutOrganizationConformancePack (..),
    newPutOrganizationConformancePack,

    -- * Request Lenses
    putOrganizationConformancePack_conformancePackInputParameters,
    putOrganizationConformancePack_deliveryS3Bucket,
    putOrganizationConformancePack_deliveryS3KeyPrefix,
    putOrganizationConformancePack_excludedAccounts,
    putOrganizationConformancePack_templateBody,
    putOrganizationConformancePack_templateS3Uri,
    putOrganizationConformancePack_organizationConformancePackName,

    -- * Destructuring the Response
    PutOrganizationConformancePackResponse (..),
    newPutOrganizationConformancePackResponse,

    -- * Response Lenses
    putOrganizationConformancePackResponse_organizationConformancePackArn,
    putOrganizationConformancePackResponse_httpStatus,
  )
where

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutOrganizationConformancePack' smart constructor.
data PutOrganizationConformancePack = PutOrganizationConformancePack'
  { -- | A list of @ConformancePackInputParameter@ objects.
    conformancePackInputParameters :: Prelude.Maybe [ConformancePackInputParameter],
    -- | The name of the Amazon S3 bucket where Config stores conformance pack
    -- templates.
    --
    -- This field is optional. If used, it must be prefixed with
    -- @awsconfigconforms@.
    deliveryS3Bucket :: Prelude.Maybe Prelude.Text,
    -- | The prefix for the Amazon S3 bucket.
    --
    -- This field is optional.
    deliveryS3KeyPrefix :: Prelude.Maybe Prelude.Text,
    -- | A list of Amazon Web Services accounts to be excluded from an
    -- organization conformance pack while deploying a conformance pack.
    excludedAccounts :: Prelude.Maybe [Prelude.Text],
    -- | A string containing full conformance pack template body. Structure
    -- containing the template body with a minimum length of 1 byte and a
    -- maximum length of 51,200 bytes.
    templateBody :: Prelude.Maybe Prelude.Text,
    -- | Location of file containing the template body. The uri must point to the
    -- conformance pack template (max size: 300 KB).
    --
    -- You must have access to read Amazon S3 bucket.
    templateS3Uri :: Prelude.Maybe Prelude.Text,
    -- | Name of the organization conformance pack you want to create.
    organizationConformancePackName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutOrganizationConformancePack' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conformancePackInputParameters', 'putOrganizationConformancePack_conformancePackInputParameters' - A list of @ConformancePackInputParameter@ objects.
--
-- 'deliveryS3Bucket', 'putOrganizationConformancePack_deliveryS3Bucket' - The name of the Amazon S3 bucket where Config stores conformance pack
-- templates.
--
-- This field is optional. If used, it must be prefixed with
-- @awsconfigconforms@.
--
-- 'deliveryS3KeyPrefix', 'putOrganizationConformancePack_deliveryS3KeyPrefix' - The prefix for the Amazon S3 bucket.
--
-- This field is optional.
--
-- 'excludedAccounts', 'putOrganizationConformancePack_excludedAccounts' - A list of Amazon Web Services accounts to be excluded from an
-- organization conformance pack while deploying a conformance pack.
--
-- 'templateBody', 'putOrganizationConformancePack_templateBody' - A string containing full conformance pack template body. Structure
-- containing the template body with a minimum length of 1 byte and a
-- maximum length of 51,200 bytes.
--
-- 'templateS3Uri', 'putOrganizationConformancePack_templateS3Uri' - Location of file containing the template body. The uri must point to the
-- conformance pack template (max size: 300 KB).
--
-- You must have access to read Amazon S3 bucket.
--
-- 'organizationConformancePackName', 'putOrganizationConformancePack_organizationConformancePackName' - Name of the organization conformance pack you want to create.
newPutOrganizationConformancePack ::
  -- | 'organizationConformancePackName'
  Prelude.Text ->
  PutOrganizationConformancePack
newPutOrganizationConformancePack
  pOrganizationConformancePackName_ =
    PutOrganizationConformancePack'
      { conformancePackInputParameters =
          Prelude.Nothing,
        deliveryS3Bucket = Prelude.Nothing,
        deliveryS3KeyPrefix = Prelude.Nothing,
        excludedAccounts = Prelude.Nothing,
        templateBody = Prelude.Nothing,
        templateS3Uri = Prelude.Nothing,
        organizationConformancePackName =
          pOrganizationConformancePackName_
      }

-- | A list of @ConformancePackInputParameter@ objects.
putOrganizationConformancePack_conformancePackInputParameters :: Lens.Lens' PutOrganizationConformancePack (Prelude.Maybe [ConformancePackInputParameter])
putOrganizationConformancePack_conformancePackInputParameters = Lens.lens (\PutOrganizationConformancePack' {conformancePackInputParameters} -> conformancePackInputParameters) (\s@PutOrganizationConformancePack' {} a -> s {conformancePackInputParameters = a} :: PutOrganizationConformancePack) Prelude.. Lens.mapping Lens.coerced

-- | The name of the Amazon S3 bucket where Config stores conformance pack
-- templates.
--
-- This field is optional. If used, it must be prefixed with
-- @awsconfigconforms@.
putOrganizationConformancePack_deliveryS3Bucket :: Lens.Lens' PutOrganizationConformancePack (Prelude.Maybe Prelude.Text)
putOrganizationConformancePack_deliveryS3Bucket = Lens.lens (\PutOrganizationConformancePack' {deliveryS3Bucket} -> deliveryS3Bucket) (\s@PutOrganizationConformancePack' {} a -> s {deliveryS3Bucket = a} :: PutOrganizationConformancePack)

-- | The prefix for the Amazon S3 bucket.
--
-- This field is optional.
putOrganizationConformancePack_deliveryS3KeyPrefix :: Lens.Lens' PutOrganizationConformancePack (Prelude.Maybe Prelude.Text)
putOrganizationConformancePack_deliveryS3KeyPrefix = Lens.lens (\PutOrganizationConformancePack' {deliveryS3KeyPrefix} -> deliveryS3KeyPrefix) (\s@PutOrganizationConformancePack' {} a -> s {deliveryS3KeyPrefix = a} :: PutOrganizationConformancePack)

-- | A list of Amazon Web Services accounts to be excluded from an
-- organization conformance pack while deploying a conformance pack.
putOrganizationConformancePack_excludedAccounts :: Lens.Lens' PutOrganizationConformancePack (Prelude.Maybe [Prelude.Text])
putOrganizationConformancePack_excludedAccounts = Lens.lens (\PutOrganizationConformancePack' {excludedAccounts} -> excludedAccounts) (\s@PutOrganizationConformancePack' {} a -> s {excludedAccounts = a} :: PutOrganizationConformancePack) Prelude.. Lens.mapping Lens.coerced

-- | A string containing full conformance pack template body. Structure
-- containing the template body with a minimum length of 1 byte and a
-- maximum length of 51,200 bytes.
putOrganizationConformancePack_templateBody :: Lens.Lens' PutOrganizationConformancePack (Prelude.Maybe Prelude.Text)
putOrganizationConformancePack_templateBody = Lens.lens (\PutOrganizationConformancePack' {templateBody} -> templateBody) (\s@PutOrganizationConformancePack' {} a -> s {templateBody = a} :: PutOrganizationConformancePack)

-- | Location of file containing the template body. The uri must point to the
-- conformance pack template (max size: 300 KB).
--
-- You must have access to read Amazon S3 bucket.
putOrganizationConformancePack_templateS3Uri :: Lens.Lens' PutOrganizationConformancePack (Prelude.Maybe Prelude.Text)
putOrganizationConformancePack_templateS3Uri = Lens.lens (\PutOrganizationConformancePack' {templateS3Uri} -> templateS3Uri) (\s@PutOrganizationConformancePack' {} a -> s {templateS3Uri = a} :: PutOrganizationConformancePack)

-- | Name of the organization conformance pack you want to create.
putOrganizationConformancePack_organizationConformancePackName :: Lens.Lens' PutOrganizationConformancePack Prelude.Text
putOrganizationConformancePack_organizationConformancePackName = Lens.lens (\PutOrganizationConformancePack' {organizationConformancePackName} -> organizationConformancePackName) (\s@PutOrganizationConformancePack' {} a -> s {organizationConformancePackName = a} :: PutOrganizationConformancePack)

instance
  Core.AWSRequest
    PutOrganizationConformancePack
  where
  type
    AWSResponse PutOrganizationConformancePack =
      PutOrganizationConformancePackResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutOrganizationConformancePackResponse'
            Prelude.<$> (x Data..?> "OrganizationConformancePackArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PutOrganizationConformancePack
  where
  hashWithSalt
    _salt
    PutOrganizationConformancePack' {..} =
      _salt
        `Prelude.hashWithSalt` conformancePackInputParameters
        `Prelude.hashWithSalt` deliveryS3Bucket
        `Prelude.hashWithSalt` deliveryS3KeyPrefix
        `Prelude.hashWithSalt` excludedAccounts
        `Prelude.hashWithSalt` templateBody
        `Prelude.hashWithSalt` templateS3Uri
        `Prelude.hashWithSalt` organizationConformancePackName

instance
  Prelude.NFData
    PutOrganizationConformancePack
  where
  rnf PutOrganizationConformancePack' {..} =
    Prelude.rnf conformancePackInputParameters `Prelude.seq`
      Prelude.rnf deliveryS3Bucket `Prelude.seq`
        Prelude.rnf deliveryS3KeyPrefix `Prelude.seq`
          Prelude.rnf excludedAccounts `Prelude.seq`
            Prelude.rnf templateBody `Prelude.seq`
              Prelude.rnf templateS3Uri `Prelude.seq`
                Prelude.rnf organizationConformancePackName

instance
  Data.ToHeaders
    PutOrganizationConformancePack
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StarlingDoveService.PutOrganizationConformancePack" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutOrganizationConformancePack where
  toJSON PutOrganizationConformancePack' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ConformancePackInputParameters" Data..=)
              Prelude.<$> conformancePackInputParameters,
            ("DeliveryS3Bucket" Data..=)
              Prelude.<$> deliveryS3Bucket,
            ("DeliveryS3KeyPrefix" Data..=)
              Prelude.<$> deliveryS3KeyPrefix,
            ("ExcludedAccounts" Data..=)
              Prelude.<$> excludedAccounts,
            ("TemplateBody" Data..=) Prelude.<$> templateBody,
            ("TemplateS3Uri" Data..=) Prelude.<$> templateS3Uri,
            Prelude.Just
              ( "OrganizationConformancePackName"
                  Data..= organizationConformancePackName
              )
          ]
      )

instance Data.ToPath PutOrganizationConformancePack where
  toPath = Prelude.const "/"

instance Data.ToQuery PutOrganizationConformancePack where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutOrganizationConformancePackResponse' smart constructor.
data PutOrganizationConformancePackResponse = PutOrganizationConformancePackResponse'
  { -- | ARN of the organization conformance pack.
    organizationConformancePackArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  PutOrganizationConformancePackResponse
newPutOrganizationConformancePackResponse
  pHttpStatus_ =
    PutOrganizationConformancePackResponse'
      { organizationConformancePackArn =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | ARN of the organization conformance pack.
putOrganizationConformancePackResponse_organizationConformancePackArn :: Lens.Lens' PutOrganizationConformancePackResponse (Prelude.Maybe Prelude.Text)
putOrganizationConformancePackResponse_organizationConformancePackArn = Lens.lens (\PutOrganizationConformancePackResponse' {organizationConformancePackArn} -> organizationConformancePackArn) (\s@PutOrganizationConformancePackResponse' {} a -> s {organizationConformancePackArn = a} :: PutOrganizationConformancePackResponse)

-- | The response's http status code.
putOrganizationConformancePackResponse_httpStatus :: Lens.Lens' PutOrganizationConformancePackResponse Prelude.Int
putOrganizationConformancePackResponse_httpStatus = Lens.lens (\PutOrganizationConformancePackResponse' {httpStatus} -> httpStatus) (\s@PutOrganizationConformancePackResponse' {} a -> s {httpStatus = a} :: PutOrganizationConformancePackResponse)

instance
  Prelude.NFData
    PutOrganizationConformancePackResponse
  where
  rnf PutOrganizationConformancePackResponse' {..} =
    Prelude.rnf organizationConformancePackArn `Prelude.seq`
      Prelude.rnf httpStatus
