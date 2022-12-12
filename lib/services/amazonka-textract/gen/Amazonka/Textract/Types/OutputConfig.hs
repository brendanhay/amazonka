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
-- Module      : Amazonka.Textract.Types.OutputConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Textract.Types.OutputConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Sets whether or not your output will go to a user created bucket. Used
-- to set the name of the bucket, and the prefix on the output file.
--
-- @OutputConfig@ is an optional parameter which lets you adjust where your
-- output will be placed. By default, Amazon Textract will store the
-- results internally and can only be accessed by the Get API operations.
-- With @OutputConfig@ enabled, you can set the name of the bucket the
-- output will be sent to the file prefix of the results where you can
-- download your results. Additionally, you can set the @KMSKeyID@
-- parameter to a customer master key (CMK) to encrypt your output. Without
-- this parameter set Amazon Textract will encrypt server-side using the
-- AWS managed CMK for Amazon S3.
--
-- Decryption of Customer Content is necessary for processing of the
-- documents by Amazon Textract. If your account is opted out under an AI
-- services opt out policy then all unencrypted Customer Content is
-- immediately and permanently deleted after the Customer Content has been
-- processed by the service. No copy of of the output is retained by Amazon
-- Textract. For information about how to opt out, see
-- <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_ai-opt-out.html Managing AI services opt-out policy.>
--
-- For more information on data privacy, see the
-- <https://aws.amazon.com/compliance/data-privacy-faq/ Data Privacy FAQ>.
--
-- /See:/ 'newOutputConfig' smart constructor.
data OutputConfig = OutputConfig'
  { -- | The prefix of the object key that the output will be saved to. When not
    -- enabled, the prefix will be “textract_output\".
    s3Prefix :: Prelude.Maybe Prelude.Text,
    -- | The name of the bucket your output will go to.
    s3Bucket :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OutputConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Prefix', 'outputConfig_s3Prefix' - The prefix of the object key that the output will be saved to. When not
-- enabled, the prefix will be “textract_output\".
--
-- 's3Bucket', 'outputConfig_s3Bucket' - The name of the bucket your output will go to.
newOutputConfig ::
  -- | 's3Bucket'
  Prelude.Text ->
  OutputConfig
newOutputConfig pS3Bucket_ =
  OutputConfig'
    { s3Prefix = Prelude.Nothing,
      s3Bucket = pS3Bucket_
    }

-- | The prefix of the object key that the output will be saved to. When not
-- enabled, the prefix will be “textract_output\".
outputConfig_s3Prefix :: Lens.Lens' OutputConfig (Prelude.Maybe Prelude.Text)
outputConfig_s3Prefix = Lens.lens (\OutputConfig' {s3Prefix} -> s3Prefix) (\s@OutputConfig' {} a -> s {s3Prefix = a} :: OutputConfig)

-- | The name of the bucket your output will go to.
outputConfig_s3Bucket :: Lens.Lens' OutputConfig Prelude.Text
outputConfig_s3Bucket = Lens.lens (\OutputConfig' {s3Bucket} -> s3Bucket) (\s@OutputConfig' {} a -> s {s3Bucket = a} :: OutputConfig)

instance Prelude.Hashable OutputConfig where
  hashWithSalt _salt OutputConfig' {..} =
    _salt `Prelude.hashWithSalt` s3Prefix
      `Prelude.hashWithSalt` s3Bucket

instance Prelude.NFData OutputConfig where
  rnf OutputConfig' {..} =
    Prelude.rnf s3Prefix
      `Prelude.seq` Prelude.rnf s3Bucket

instance Data.ToJSON OutputConfig where
  toJSON OutputConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("S3Prefix" Data..=) Prelude.<$> s3Prefix,
            Prelude.Just ("S3Bucket" Data..= s3Bucket)
          ]
      )
