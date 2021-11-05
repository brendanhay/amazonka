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
-- Module      : Amazonka.Synthetics.Types.CanaryCodeInput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Synthetics.Types.CanaryCodeInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Use this structure to input your script code for the canary. This
-- structure contains the Lambda handler with the location where the canary
-- should start running the script. If the script is stored in an S3
-- bucket, the bucket name, key, and version are also included. If the
-- script was passed into the canary directly, the script code is contained
-- in the value of @Zipfile@.
--
-- /See:/ 'newCanaryCodeInput' smart constructor.
data CanaryCodeInput = CanaryCodeInput'
  { -- | The S3 key of your script. For more information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingObjects.html Working with Amazon S3 Objects>.
    s3Key :: Prelude.Maybe Prelude.Text,
    -- | The S3 version ID of your script.
    s3Version :: Prelude.Maybe Prelude.Text,
    -- | If you input your canary script directly into the canary instead of
    -- referring to an S3 location, the value of this parameter is the
    -- base64-encoded contents of the .zip file that contains the script. It
    -- must be smaller than 256 Kb.
    zipFile :: Prelude.Maybe Core.Base64,
    -- | If your canary script is located in S3, specify the bucket name here. Do
    -- not include @s3:\/\/@ as the start of the bucket name.
    s3Bucket :: Prelude.Maybe Prelude.Text,
    -- | The entry point to use for the source code when running the canary. This
    -- value must end with the string @.handler@. The string is limited to 29
    -- characters or fewer.
    handler :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CanaryCodeInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Key', 'canaryCodeInput_s3Key' - The S3 key of your script. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingObjects.html Working with Amazon S3 Objects>.
--
-- 's3Version', 'canaryCodeInput_s3Version' - The S3 version ID of your script.
--
-- 'zipFile', 'canaryCodeInput_zipFile' - If you input your canary script directly into the canary instead of
-- referring to an S3 location, the value of this parameter is the
-- base64-encoded contents of the .zip file that contains the script. It
-- must be smaller than 256 Kb.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 's3Bucket', 'canaryCodeInput_s3Bucket' - If your canary script is located in S3, specify the bucket name here. Do
-- not include @s3:\/\/@ as the start of the bucket name.
--
-- 'handler', 'canaryCodeInput_handler' - The entry point to use for the source code when running the canary. This
-- value must end with the string @.handler@. The string is limited to 29
-- characters or fewer.
newCanaryCodeInput ::
  -- | 'handler'
  Prelude.Text ->
  CanaryCodeInput
newCanaryCodeInput pHandler_ =
  CanaryCodeInput'
    { s3Key = Prelude.Nothing,
      s3Version = Prelude.Nothing,
      zipFile = Prelude.Nothing,
      s3Bucket = Prelude.Nothing,
      handler = pHandler_
    }

-- | The S3 key of your script. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingObjects.html Working with Amazon S3 Objects>.
canaryCodeInput_s3Key :: Lens.Lens' CanaryCodeInput (Prelude.Maybe Prelude.Text)
canaryCodeInput_s3Key = Lens.lens (\CanaryCodeInput' {s3Key} -> s3Key) (\s@CanaryCodeInput' {} a -> s {s3Key = a} :: CanaryCodeInput)

-- | The S3 version ID of your script.
canaryCodeInput_s3Version :: Lens.Lens' CanaryCodeInput (Prelude.Maybe Prelude.Text)
canaryCodeInput_s3Version = Lens.lens (\CanaryCodeInput' {s3Version} -> s3Version) (\s@CanaryCodeInput' {} a -> s {s3Version = a} :: CanaryCodeInput)

-- | If you input your canary script directly into the canary instead of
-- referring to an S3 location, the value of this parameter is the
-- base64-encoded contents of the .zip file that contains the script. It
-- must be smaller than 256 Kb.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
canaryCodeInput_zipFile :: Lens.Lens' CanaryCodeInput (Prelude.Maybe Prelude.ByteString)
canaryCodeInput_zipFile = Lens.lens (\CanaryCodeInput' {zipFile} -> zipFile) (\s@CanaryCodeInput' {} a -> s {zipFile = a} :: CanaryCodeInput) Prelude.. Lens.mapping Core._Base64

-- | If your canary script is located in S3, specify the bucket name here. Do
-- not include @s3:\/\/@ as the start of the bucket name.
canaryCodeInput_s3Bucket :: Lens.Lens' CanaryCodeInput (Prelude.Maybe Prelude.Text)
canaryCodeInput_s3Bucket = Lens.lens (\CanaryCodeInput' {s3Bucket} -> s3Bucket) (\s@CanaryCodeInput' {} a -> s {s3Bucket = a} :: CanaryCodeInput)

-- | The entry point to use for the source code when running the canary. This
-- value must end with the string @.handler@. The string is limited to 29
-- characters or fewer.
canaryCodeInput_handler :: Lens.Lens' CanaryCodeInput Prelude.Text
canaryCodeInput_handler = Lens.lens (\CanaryCodeInput' {handler} -> handler) (\s@CanaryCodeInput' {} a -> s {handler = a} :: CanaryCodeInput)

instance Prelude.Hashable CanaryCodeInput

instance Prelude.NFData CanaryCodeInput

instance Core.ToJSON CanaryCodeInput where
  toJSON CanaryCodeInput' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("S3Key" Core..=) Prelude.<$> s3Key,
            ("S3Version" Core..=) Prelude.<$> s3Version,
            ("ZipFile" Core..=) Prelude.<$> zipFile,
            ("S3Bucket" Core..=) Prelude.<$> s3Bucket,
            Prelude.Just ("Handler" Core..= handler)
          ]
      )
