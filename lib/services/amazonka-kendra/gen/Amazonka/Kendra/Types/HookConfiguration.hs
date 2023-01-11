{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Kendra.Types.HookConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.HookConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.DocumentAttributeCondition
import qualified Amazonka.Prelude as Prelude

-- | Provides the configuration information for invoking a Lambda function in
-- Lambda to alter document metadata and content when ingesting documents
-- into Amazon Kendra. You can configure your Lambda function using
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_CustomDocumentEnrichmentConfiguration.html PreExtractionHookConfiguration>
-- if you want to apply advanced alterations on the original or raw
-- documents. If you want to apply advanced alterations on the Amazon
-- Kendra structured documents, you must configure your Lambda function
-- using
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_CustomDocumentEnrichmentConfiguration.html PostExtractionHookConfiguration>.
-- You can only invoke one Lambda function. However, this function can
-- invoke other functions it requires.
--
-- For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/custom-document-enrichment.html Customizing document metadata during the ingestion process>.
--
-- /See:/ 'newHookConfiguration' smart constructor.
data HookConfiguration = HookConfiguration'
  { -- | The condition used for when a Lambda function should be invoked.
    --
    -- For example, you can specify a condition that if there are empty
    -- date-time values, then Amazon Kendra should invoke a function that
    -- inserts the current date-time.
    invocationCondition :: Prelude.Maybe DocumentAttributeCondition,
    -- | The Amazon Resource Name (ARN) of a role with permission to run a Lambda
    -- function during ingestion. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/iam-roles.html IAM roles for Amazon Kendra>.
    lambdaArn :: Prelude.Text,
    -- | Stores the original, raw documents or the structured, parsed documents
    -- before and after altering them. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/custom-document-enrichment.html#cde-data-contracts-lambda Data contracts for Lambda functions>.
    s3Bucket :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HookConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'invocationCondition', 'hookConfiguration_invocationCondition' - The condition used for when a Lambda function should be invoked.
--
-- For example, you can specify a condition that if there are empty
-- date-time values, then Amazon Kendra should invoke a function that
-- inserts the current date-time.
--
-- 'lambdaArn', 'hookConfiguration_lambdaArn' - The Amazon Resource Name (ARN) of a role with permission to run a Lambda
-- function during ingestion. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/iam-roles.html IAM roles for Amazon Kendra>.
--
-- 's3Bucket', 'hookConfiguration_s3Bucket' - Stores the original, raw documents or the structured, parsed documents
-- before and after altering them. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/custom-document-enrichment.html#cde-data-contracts-lambda Data contracts for Lambda functions>.
newHookConfiguration ::
  -- | 'lambdaArn'
  Prelude.Text ->
  -- | 's3Bucket'
  Prelude.Text ->
  HookConfiguration
newHookConfiguration pLambdaArn_ pS3Bucket_ =
  HookConfiguration'
    { invocationCondition =
        Prelude.Nothing,
      lambdaArn = pLambdaArn_,
      s3Bucket = pS3Bucket_
    }

-- | The condition used for when a Lambda function should be invoked.
--
-- For example, you can specify a condition that if there are empty
-- date-time values, then Amazon Kendra should invoke a function that
-- inserts the current date-time.
hookConfiguration_invocationCondition :: Lens.Lens' HookConfiguration (Prelude.Maybe DocumentAttributeCondition)
hookConfiguration_invocationCondition = Lens.lens (\HookConfiguration' {invocationCondition} -> invocationCondition) (\s@HookConfiguration' {} a -> s {invocationCondition = a} :: HookConfiguration)

-- | The Amazon Resource Name (ARN) of a role with permission to run a Lambda
-- function during ingestion. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/iam-roles.html IAM roles for Amazon Kendra>.
hookConfiguration_lambdaArn :: Lens.Lens' HookConfiguration Prelude.Text
hookConfiguration_lambdaArn = Lens.lens (\HookConfiguration' {lambdaArn} -> lambdaArn) (\s@HookConfiguration' {} a -> s {lambdaArn = a} :: HookConfiguration)

-- | Stores the original, raw documents or the structured, parsed documents
-- before and after altering them. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/custom-document-enrichment.html#cde-data-contracts-lambda Data contracts for Lambda functions>.
hookConfiguration_s3Bucket :: Lens.Lens' HookConfiguration Prelude.Text
hookConfiguration_s3Bucket = Lens.lens (\HookConfiguration' {s3Bucket} -> s3Bucket) (\s@HookConfiguration' {} a -> s {s3Bucket = a} :: HookConfiguration)

instance Data.FromJSON HookConfiguration where
  parseJSON =
    Data.withObject
      "HookConfiguration"
      ( \x ->
          HookConfiguration'
            Prelude.<$> (x Data..:? "InvocationCondition")
            Prelude.<*> (x Data..: "LambdaArn")
            Prelude.<*> (x Data..: "S3Bucket")
      )

instance Prelude.Hashable HookConfiguration where
  hashWithSalt _salt HookConfiguration' {..} =
    _salt `Prelude.hashWithSalt` invocationCondition
      `Prelude.hashWithSalt` lambdaArn
      `Prelude.hashWithSalt` s3Bucket

instance Prelude.NFData HookConfiguration where
  rnf HookConfiguration' {..} =
    Prelude.rnf invocationCondition
      `Prelude.seq` Prelude.rnf lambdaArn
      `Prelude.seq` Prelude.rnf s3Bucket

instance Data.ToJSON HookConfiguration where
  toJSON HookConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("InvocationCondition" Data..=)
              Prelude.<$> invocationCondition,
            Prelude.Just ("LambdaArn" Data..= lambdaArn),
            Prelude.Just ("S3Bucket" Data..= s3Bucket)
          ]
      )
