{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Config.PutConformancePack
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a conformance pack. A conformance pack is a
-- collection of AWS Config rules that can be easily deployed in an account
-- and a region and across AWS Organization.
--
-- This API creates a service linked role @AWSServiceRoleForConfigConforms@
-- in your account. The service linked role is created only when the role
-- does not exist in your account.
--
-- You must specify either the @TemplateS3Uri@ or the @TemplateBody@
-- parameter, but not both. If you provide both AWS Config uses the
-- @TemplateS3Uri@ parameter and ignores the @TemplateBody@ parameter.
module Network.AWS.Config.PutConformancePack
  ( -- * Creating a Request
    PutConformancePack (..),
    newPutConformancePack,

    -- * Request Lenses
    putConformancePack_templateS3Uri,
    putConformancePack_deliveryS3Bucket,
    putConformancePack_deliveryS3KeyPrefix,
    putConformancePack_templateBody,
    putConformancePack_conformancePackInputParameters,
    putConformancePack_conformancePackName,

    -- * Destructuring the Response
    PutConformancePackResponse (..),
    newPutConformancePackResponse,

    -- * Response Lenses
    putConformancePackResponse_conformancePackArn,
    putConformancePackResponse_httpStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutConformancePack' smart constructor.
data PutConformancePack = PutConformancePack'
  { -- | Location of file containing the template body
    -- (@s3:\/\/bucketname\/prefix@). The uri must point to the conformance
    -- pack template (max size: 300 KB) that is located in an Amazon S3 bucket
    -- in the same region as the conformance pack.
    --
    -- You must have access to read Amazon S3 bucket.
    templateS3Uri :: Prelude.Maybe Prelude.Text,
    -- | Amazon S3 bucket where AWS Config stores conformance pack templates.
    --
    -- This field is optional.
    deliveryS3Bucket :: Prelude.Maybe Prelude.Text,
    -- | The prefix for the Amazon S3 bucket.
    --
    -- This field is optional.
    deliveryS3KeyPrefix :: Prelude.Maybe Prelude.Text,
    -- | A string containing full conformance pack template body. Structure
    -- containing the template body with a minimum length of 1 byte and a
    -- maximum length of 51,200 bytes.
    --
    -- You can only use a YAML template with one resource type, that is, config
    -- rule and a remediation action.
    templateBody :: Prelude.Maybe Prelude.Text,
    -- | A list of @ConformancePackInputParameter@ objects.
    conformancePackInputParameters :: Prelude.Maybe [ConformancePackInputParameter],
    -- | Name of the conformance pack you want to create.
    conformancePackName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutConformancePack' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateS3Uri', 'putConformancePack_templateS3Uri' - Location of file containing the template body
-- (@s3:\/\/bucketname\/prefix@). The uri must point to the conformance
-- pack template (max size: 300 KB) that is located in an Amazon S3 bucket
-- in the same region as the conformance pack.
--
-- You must have access to read Amazon S3 bucket.
--
-- 'deliveryS3Bucket', 'putConformancePack_deliveryS3Bucket' - Amazon S3 bucket where AWS Config stores conformance pack templates.
--
-- This field is optional.
--
-- 'deliveryS3KeyPrefix', 'putConformancePack_deliveryS3KeyPrefix' - The prefix for the Amazon S3 bucket.
--
-- This field is optional.
--
-- 'templateBody', 'putConformancePack_templateBody' - A string containing full conformance pack template body. Structure
-- containing the template body with a minimum length of 1 byte and a
-- maximum length of 51,200 bytes.
--
-- You can only use a YAML template with one resource type, that is, config
-- rule and a remediation action.
--
-- 'conformancePackInputParameters', 'putConformancePack_conformancePackInputParameters' - A list of @ConformancePackInputParameter@ objects.
--
-- 'conformancePackName', 'putConformancePack_conformancePackName' - Name of the conformance pack you want to create.
newPutConformancePack ::
  -- | 'conformancePackName'
  Prelude.Text ->
  PutConformancePack
newPutConformancePack pConformancePackName_ =
  PutConformancePack'
    { templateS3Uri =
        Prelude.Nothing,
      deliveryS3Bucket = Prelude.Nothing,
      deliveryS3KeyPrefix = Prelude.Nothing,
      templateBody = Prelude.Nothing,
      conformancePackInputParameters = Prelude.Nothing,
      conformancePackName = pConformancePackName_
    }

-- | Location of file containing the template body
-- (@s3:\/\/bucketname\/prefix@). The uri must point to the conformance
-- pack template (max size: 300 KB) that is located in an Amazon S3 bucket
-- in the same region as the conformance pack.
--
-- You must have access to read Amazon S3 bucket.
putConformancePack_templateS3Uri :: Lens.Lens' PutConformancePack (Prelude.Maybe Prelude.Text)
putConformancePack_templateS3Uri = Lens.lens (\PutConformancePack' {templateS3Uri} -> templateS3Uri) (\s@PutConformancePack' {} a -> s {templateS3Uri = a} :: PutConformancePack)

-- | Amazon S3 bucket where AWS Config stores conformance pack templates.
--
-- This field is optional.
putConformancePack_deliveryS3Bucket :: Lens.Lens' PutConformancePack (Prelude.Maybe Prelude.Text)
putConformancePack_deliveryS3Bucket = Lens.lens (\PutConformancePack' {deliveryS3Bucket} -> deliveryS3Bucket) (\s@PutConformancePack' {} a -> s {deliveryS3Bucket = a} :: PutConformancePack)

-- | The prefix for the Amazon S3 bucket.
--
-- This field is optional.
putConformancePack_deliveryS3KeyPrefix :: Lens.Lens' PutConformancePack (Prelude.Maybe Prelude.Text)
putConformancePack_deliveryS3KeyPrefix = Lens.lens (\PutConformancePack' {deliveryS3KeyPrefix} -> deliveryS3KeyPrefix) (\s@PutConformancePack' {} a -> s {deliveryS3KeyPrefix = a} :: PutConformancePack)

-- | A string containing full conformance pack template body. Structure
-- containing the template body with a minimum length of 1 byte and a
-- maximum length of 51,200 bytes.
--
-- You can only use a YAML template with one resource type, that is, config
-- rule and a remediation action.
putConformancePack_templateBody :: Lens.Lens' PutConformancePack (Prelude.Maybe Prelude.Text)
putConformancePack_templateBody = Lens.lens (\PutConformancePack' {templateBody} -> templateBody) (\s@PutConformancePack' {} a -> s {templateBody = a} :: PutConformancePack)

-- | A list of @ConformancePackInputParameter@ objects.
putConformancePack_conformancePackInputParameters :: Lens.Lens' PutConformancePack (Prelude.Maybe [ConformancePackInputParameter])
putConformancePack_conformancePackInputParameters = Lens.lens (\PutConformancePack' {conformancePackInputParameters} -> conformancePackInputParameters) (\s@PutConformancePack' {} a -> s {conformancePackInputParameters = a} :: PutConformancePack) Prelude.. Lens.mapping Prelude._Coerce

-- | Name of the conformance pack you want to create.
putConformancePack_conformancePackName :: Lens.Lens' PutConformancePack Prelude.Text
putConformancePack_conformancePackName = Lens.lens (\PutConformancePack' {conformancePackName} -> conformancePackName) (\s@PutConformancePack' {} a -> s {conformancePackName = a} :: PutConformancePack)

instance Prelude.AWSRequest PutConformancePack where
  type
    Rs PutConformancePack =
      PutConformancePackResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutConformancePackResponse'
            Prelude.<$> (x Prelude..?> "ConformancePackArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutConformancePack

instance Prelude.NFData PutConformancePack

instance Prelude.ToHeaders PutConformancePack where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "StarlingDoveService.PutConformancePack" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON PutConformancePack where
  toJSON PutConformancePack' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("TemplateS3Uri" Prelude..=)
              Prelude.<$> templateS3Uri,
            ("DeliveryS3Bucket" Prelude..=)
              Prelude.<$> deliveryS3Bucket,
            ("DeliveryS3KeyPrefix" Prelude..=)
              Prelude.<$> deliveryS3KeyPrefix,
            ("TemplateBody" Prelude..=) Prelude.<$> templateBody,
            ("ConformancePackInputParameters" Prelude..=)
              Prelude.<$> conformancePackInputParameters,
            Prelude.Just
              ( "ConformancePackName"
                  Prelude..= conformancePackName
              )
          ]
      )

instance Prelude.ToPath PutConformancePack where
  toPath = Prelude.const "/"

instance Prelude.ToQuery PutConformancePack where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutConformancePackResponse' smart constructor.
data PutConformancePackResponse = PutConformancePackResponse'
  { -- | ARN of the conformance pack.
    conformancePackArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData PutConformancePackResponse
