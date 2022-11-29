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
-- Module      : Amazonka.Config.PutConformancePack
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a conformance pack. A conformance pack is a
-- collection of Config rules that can be easily deployed in an account and
-- a region and across an organization. For information on how many
-- conformance packs you can have per account, see
-- <https://docs.aws.amazon.com/config/latest/developerguide/configlimits.html Service Limits>
-- in the Config Developer Guide.
--
-- This API creates a service-linked role @AWSServiceRoleForConfigConforms@
-- in your account. The service-linked role is created only when the role
-- does not exist in your account.
--
-- You must specify only one of the follow parameters: @TemplateS3Uri@,
-- @TemplateBody@ or @TemplateSSMDocumentDetails@.
module Amazonka.Config.PutConformancePack
  ( -- * Creating a Request
    PutConformancePack (..),
    newPutConformancePack,

    -- * Request Lenses
    putConformancePack_templateS3Uri,
    putConformancePack_conformancePackInputParameters,
    putConformancePack_templateBody,
    putConformancePack_deliveryS3Bucket,
    putConformancePack_templateSSMDocumentDetails,
    putConformancePack_deliveryS3KeyPrefix,
    putConformancePack_conformancePackName,

    -- * Destructuring the Response
    PutConformancePackResponse (..),
    newPutConformancePackResponse,

    -- * Response Lenses
    putConformancePackResponse_conformancePackArn,
    putConformancePackResponse_httpStatus,
  )
where

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutConformancePack' smart constructor.
data PutConformancePack = PutConformancePack'
  { -- | The location of the file containing the template body
    -- (@s3:\/\/bucketname\/prefix@). The uri must point to a conformance pack
    -- template (max size: 300 KB) that is located in an Amazon S3 bucket in
    -- the same Region as the conformance pack.
    --
    -- You must have access to read Amazon S3 bucket.
    templateS3Uri :: Prelude.Maybe Prelude.Text,
    -- | A list of @ConformancePackInputParameter@ objects.
    conformancePackInputParameters :: Prelude.Maybe [ConformancePackInputParameter],
    -- | A string containing the full conformance pack template body. The
    -- structure containing the template body has a minimum length of 1 byte
    -- and a maximum length of 51,200 bytes.
    --
    -- You can use a YAML template with two resource types: Config rule
    -- (@AWS::Config::ConfigRule@) and remediation action
    -- (@AWS::Config::RemediationConfiguration@).
    templateBody :: Prelude.Maybe Prelude.Text,
    -- | The name of the Amazon S3 bucket where Config stores conformance pack
    -- templates.
    --
    -- This field is optional.
    deliveryS3Bucket :: Prelude.Maybe Prelude.Text,
    -- | An object of type @TemplateSSMDocumentDetails@, which contains the name
    -- or the Amazon Resource Name (ARN) of the Amazon Web Services Systems
    -- Manager document (SSM document) and the version of the SSM document that
    -- is used to create a conformance pack.
    templateSSMDocumentDetails :: Prelude.Maybe TemplateSSMDocumentDetails,
    -- | The prefix for the Amazon S3 bucket.
    --
    -- This field is optional.
    deliveryS3KeyPrefix :: Prelude.Maybe Prelude.Text,
    -- | The unique name of the conformance pack you want to deploy.
    conformancePackName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutConformancePack' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateS3Uri', 'putConformancePack_templateS3Uri' - The location of the file containing the template body
-- (@s3:\/\/bucketname\/prefix@). The uri must point to a conformance pack
-- template (max size: 300 KB) that is located in an Amazon S3 bucket in
-- the same Region as the conformance pack.
--
-- You must have access to read Amazon S3 bucket.
--
-- 'conformancePackInputParameters', 'putConformancePack_conformancePackInputParameters' - A list of @ConformancePackInputParameter@ objects.
--
-- 'templateBody', 'putConformancePack_templateBody' - A string containing the full conformance pack template body. The
-- structure containing the template body has a minimum length of 1 byte
-- and a maximum length of 51,200 bytes.
--
-- You can use a YAML template with two resource types: Config rule
-- (@AWS::Config::ConfigRule@) and remediation action
-- (@AWS::Config::RemediationConfiguration@).
--
-- 'deliveryS3Bucket', 'putConformancePack_deliveryS3Bucket' - The name of the Amazon S3 bucket where Config stores conformance pack
-- templates.
--
-- This field is optional.
--
-- 'templateSSMDocumentDetails', 'putConformancePack_templateSSMDocumentDetails' - An object of type @TemplateSSMDocumentDetails@, which contains the name
-- or the Amazon Resource Name (ARN) of the Amazon Web Services Systems
-- Manager document (SSM document) and the version of the SSM document that
-- is used to create a conformance pack.
--
-- 'deliveryS3KeyPrefix', 'putConformancePack_deliveryS3KeyPrefix' - The prefix for the Amazon S3 bucket.
--
-- This field is optional.
--
-- 'conformancePackName', 'putConformancePack_conformancePackName' - The unique name of the conformance pack you want to deploy.
newPutConformancePack ::
  -- | 'conformancePackName'
  Prelude.Text ->
  PutConformancePack
newPutConformancePack pConformancePackName_ =
  PutConformancePack'
    { templateS3Uri =
        Prelude.Nothing,
      conformancePackInputParameters = Prelude.Nothing,
      templateBody = Prelude.Nothing,
      deliveryS3Bucket = Prelude.Nothing,
      templateSSMDocumentDetails = Prelude.Nothing,
      deliveryS3KeyPrefix = Prelude.Nothing,
      conformancePackName = pConformancePackName_
    }

-- | The location of the file containing the template body
-- (@s3:\/\/bucketname\/prefix@). The uri must point to a conformance pack
-- template (max size: 300 KB) that is located in an Amazon S3 bucket in
-- the same Region as the conformance pack.
--
-- You must have access to read Amazon S3 bucket.
putConformancePack_templateS3Uri :: Lens.Lens' PutConformancePack (Prelude.Maybe Prelude.Text)
putConformancePack_templateS3Uri = Lens.lens (\PutConformancePack' {templateS3Uri} -> templateS3Uri) (\s@PutConformancePack' {} a -> s {templateS3Uri = a} :: PutConformancePack)

-- | A list of @ConformancePackInputParameter@ objects.
putConformancePack_conformancePackInputParameters :: Lens.Lens' PutConformancePack (Prelude.Maybe [ConformancePackInputParameter])
putConformancePack_conformancePackInputParameters = Lens.lens (\PutConformancePack' {conformancePackInputParameters} -> conformancePackInputParameters) (\s@PutConformancePack' {} a -> s {conformancePackInputParameters = a} :: PutConformancePack) Prelude.. Lens.mapping Lens.coerced

-- | A string containing the full conformance pack template body. The
-- structure containing the template body has a minimum length of 1 byte
-- and a maximum length of 51,200 bytes.
--
-- You can use a YAML template with two resource types: Config rule
-- (@AWS::Config::ConfigRule@) and remediation action
-- (@AWS::Config::RemediationConfiguration@).
putConformancePack_templateBody :: Lens.Lens' PutConformancePack (Prelude.Maybe Prelude.Text)
putConformancePack_templateBody = Lens.lens (\PutConformancePack' {templateBody} -> templateBody) (\s@PutConformancePack' {} a -> s {templateBody = a} :: PutConformancePack)

-- | The name of the Amazon S3 bucket where Config stores conformance pack
-- templates.
--
-- This field is optional.
putConformancePack_deliveryS3Bucket :: Lens.Lens' PutConformancePack (Prelude.Maybe Prelude.Text)
putConformancePack_deliveryS3Bucket = Lens.lens (\PutConformancePack' {deliveryS3Bucket} -> deliveryS3Bucket) (\s@PutConformancePack' {} a -> s {deliveryS3Bucket = a} :: PutConformancePack)

-- | An object of type @TemplateSSMDocumentDetails@, which contains the name
-- or the Amazon Resource Name (ARN) of the Amazon Web Services Systems
-- Manager document (SSM document) and the version of the SSM document that
-- is used to create a conformance pack.
putConformancePack_templateSSMDocumentDetails :: Lens.Lens' PutConformancePack (Prelude.Maybe TemplateSSMDocumentDetails)
putConformancePack_templateSSMDocumentDetails = Lens.lens (\PutConformancePack' {templateSSMDocumentDetails} -> templateSSMDocumentDetails) (\s@PutConformancePack' {} a -> s {templateSSMDocumentDetails = a} :: PutConformancePack)

-- | The prefix for the Amazon S3 bucket.
--
-- This field is optional.
putConformancePack_deliveryS3KeyPrefix :: Lens.Lens' PutConformancePack (Prelude.Maybe Prelude.Text)
putConformancePack_deliveryS3KeyPrefix = Lens.lens (\PutConformancePack' {deliveryS3KeyPrefix} -> deliveryS3KeyPrefix) (\s@PutConformancePack' {} a -> s {deliveryS3KeyPrefix = a} :: PutConformancePack)

-- | The unique name of the conformance pack you want to deploy.
putConformancePack_conformancePackName :: Lens.Lens' PutConformancePack Prelude.Text
putConformancePack_conformancePackName = Lens.lens (\PutConformancePack' {conformancePackName} -> conformancePackName) (\s@PutConformancePack' {} a -> s {conformancePackName = a} :: PutConformancePack)

instance Core.AWSRequest PutConformancePack where
  type
    AWSResponse PutConformancePack =
      PutConformancePackResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutConformancePackResponse'
            Prelude.<$> (x Core..?> "ConformancePackArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutConformancePack where
  hashWithSalt _salt PutConformancePack' {..} =
    _salt `Prelude.hashWithSalt` templateS3Uri
      `Prelude.hashWithSalt` conformancePackInputParameters
      `Prelude.hashWithSalt` templateBody
      `Prelude.hashWithSalt` deliveryS3Bucket
      `Prelude.hashWithSalt` templateSSMDocumentDetails
      `Prelude.hashWithSalt` deliveryS3KeyPrefix
      `Prelude.hashWithSalt` conformancePackName

instance Prelude.NFData PutConformancePack where
  rnf PutConformancePack' {..} =
    Prelude.rnf templateS3Uri
      `Prelude.seq` Prelude.rnf conformancePackInputParameters
      `Prelude.seq` Prelude.rnf templateBody
      `Prelude.seq` Prelude.rnf deliveryS3Bucket
      `Prelude.seq` Prelude.rnf templateSSMDocumentDetails
      `Prelude.seq` Prelude.rnf deliveryS3KeyPrefix
      `Prelude.seq` Prelude.rnf conformancePackName

instance Core.ToHeaders PutConformancePack where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.PutConformancePack" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON PutConformancePack where
  toJSON PutConformancePack' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("TemplateS3Uri" Core..=) Prelude.<$> templateS3Uri,
            ("ConformancePackInputParameters" Core..=)
              Prelude.<$> conformancePackInputParameters,
            ("TemplateBody" Core..=) Prelude.<$> templateBody,
            ("DeliveryS3Bucket" Core..=)
              Prelude.<$> deliveryS3Bucket,
            ("TemplateSSMDocumentDetails" Core..=)
              Prelude.<$> templateSSMDocumentDetails,
            ("DeliveryS3KeyPrefix" Core..=)
              Prelude.<$> deliveryS3KeyPrefix,
            Prelude.Just
              ("ConformancePackName" Core..= conformancePackName)
          ]
      )

instance Core.ToPath PutConformancePack where
  toPath = Prelude.const "/"

instance Core.ToQuery PutConformancePack where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutConformancePackResponse' smart constructor.
data PutConformancePackResponse = PutConformancePackResponse'
  { -- | ARN of the conformance pack.
    conformancePackArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutConformancePackResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conformancePackArn', 'putConformancePackResponse_conformancePackArn' - ARN of the conformance pack.
--
-- 'httpStatus', 'putConformancePackResponse_httpStatus' - The response's http status code.
newPutConformancePackResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutConformancePackResponse
newPutConformancePackResponse pHttpStatus_ =
  PutConformancePackResponse'
    { conformancePackArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | ARN of the conformance pack.
putConformancePackResponse_conformancePackArn :: Lens.Lens' PutConformancePackResponse (Prelude.Maybe Prelude.Text)
putConformancePackResponse_conformancePackArn = Lens.lens (\PutConformancePackResponse' {conformancePackArn} -> conformancePackArn) (\s@PutConformancePackResponse' {} a -> s {conformancePackArn = a} :: PutConformancePackResponse)

-- | The response's http status code.
putConformancePackResponse_httpStatus :: Lens.Lens' PutConformancePackResponse Prelude.Int
putConformancePackResponse_httpStatus = Lens.lens (\PutConformancePackResponse' {httpStatus} -> httpStatus) (\s@PutConformancePackResponse' {} a -> s {httpStatus = a} :: PutConformancePackResponse)

instance Prelude.NFData PutConformancePackResponse where
  rnf PutConformancePackResponse' {..} =
    Prelude.rnf conformancePackArn
      `Prelude.seq` Prelude.rnf httpStatus
