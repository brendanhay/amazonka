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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Synthetics.Types.CanaryCodeInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
  { -- | If your canary script is located in S3, specify the bucket name here. Do
    -- not include @s3:\/\/@ as the start of the bucket name.
    s3Bucket :: Prelude.Maybe Prelude.Text,
    -- | The S3 key of your script. For more information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingObjects.html Working with Amazon S3 Objects>.
    s3Key :: Prelude.Maybe Prelude.Text,
    -- | The S3 version ID of your script.
    s3Version :: Prelude.Maybe Prelude.Text,
    -- | If you input your canary script directly into the canary instead of
    -- referring to an S3 location, the value of this parameter is the
    -- base64-encoded contents of the .zip file that contains the script. It
    -- must be smaller than 225 Kb.
    --
    -- For large canary scripts, we recommend that you use an S3 location
    -- instead of inputting it directly with this parameter.
    zipFile :: Prelude.Maybe Data.Base64,
    -- | The entry point to use for the source code when running the canary. For
    -- canaries that use the @syn-python-selenium-1.0@ runtime or a
    -- @syn-nodejs.puppeteer@ runtime earlier than @syn-nodejs.puppeteer-3.4@,
    -- the handler must be specified as @ fileName.handler@. For
    -- @syn-python-selenium-1.1@, @syn-nodejs.puppeteer-3.4@, and later
    -- runtimes, the handler can be specified as @ fileName.functionName @, or
    -- you can specify a folder where canary scripts reside as
    -- @ folder\/fileName.functionName @.
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
-- 's3Bucket', 'canaryCodeInput_s3Bucket' - If your canary script is located in S3, specify the bucket name here. Do
-- not include @s3:\/\/@ as the start of the bucket name.
--
-- 's3Key', 'canaryCodeInput_s3Key' - The S3 key of your script. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingObjects.html Working with Amazon S3 Objects>.
--
-- 's3Version', 'canaryCodeInput_s3Version' - The S3 version ID of your script.
--
-- 'zipFile', 'canaryCodeInput_zipFile' - If you input your canary script directly into the canary instead of
-- referring to an S3 location, the value of this parameter is the
-- base64-encoded contents of the .zip file that contains the script. It
-- must be smaller than 225 Kb.
--
-- For large canary scripts, we recommend that you use an S3 location
-- instead of inputting it directly with this parameter.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'handler', 'canaryCodeInput_handler' - The entry point to use for the source code when running the canary. For
-- canaries that use the @syn-python-selenium-1.0@ runtime or a
-- @syn-nodejs.puppeteer@ runtime earlier than @syn-nodejs.puppeteer-3.4@,
-- the handler must be specified as @ fileName.handler@. For
-- @syn-python-selenium-1.1@, @syn-nodejs.puppeteer-3.4@, and later
-- runtimes, the handler can be specified as @ fileName.functionName @, or
-- you can specify a folder where canary scripts reside as
-- @ folder\/fileName.functionName @.
newCanaryCodeInput ::
  -- | 'handler'
  Prelude.Text ->
  CanaryCodeInput
newCanaryCodeInput pHandler_ =
  CanaryCodeInput'
    { s3Bucket = Prelude.Nothing,
      s3Key = Prelude.Nothing,
      s3Version = Prelude.Nothing,
      zipFile = Prelude.Nothing,
      handler = pHandler_
    }

-- | If your canary script is located in S3, specify the bucket name here. Do
-- not include @s3:\/\/@ as the start of the bucket name.
canaryCodeInput_s3Bucket :: Lens.Lens' CanaryCodeInput (Prelude.Maybe Prelude.Text)
canaryCodeInput_s3Bucket = Lens.lens (\CanaryCodeInput' {s3Bucket} -> s3Bucket) (\s@CanaryCodeInput' {} a -> s {s3Bucket = a} :: CanaryCodeInput)

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
-- must be smaller than 225 Kb.
--
-- For large canary scripts, we recommend that you use an S3 location
-- instead of inputting it directly with this parameter.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
canaryCodeInput_zipFile :: Lens.Lens' CanaryCodeInput (Prelude.Maybe Prelude.ByteString)
canaryCodeInput_zipFile = Lens.lens (\CanaryCodeInput' {zipFile} -> zipFile) (\s@CanaryCodeInput' {} a -> s {zipFile = a} :: CanaryCodeInput) Prelude.. Lens.mapping Data._Base64

-- | The entry point to use for the source code when running the canary. For
-- canaries that use the @syn-python-selenium-1.0@ runtime or a
-- @syn-nodejs.puppeteer@ runtime earlier than @syn-nodejs.puppeteer-3.4@,
-- the handler must be specified as @ fileName.handler@. For
-- @syn-python-selenium-1.1@, @syn-nodejs.puppeteer-3.4@, and later
-- runtimes, the handler can be specified as @ fileName.functionName @, or
-- you can specify a folder where canary scripts reside as
-- @ folder\/fileName.functionName @.
canaryCodeInput_handler :: Lens.Lens' CanaryCodeInput Prelude.Text
canaryCodeInput_handler = Lens.lens (\CanaryCodeInput' {handler} -> handler) (\s@CanaryCodeInput' {} a -> s {handler = a} :: CanaryCodeInput)

instance Prelude.Hashable CanaryCodeInput where
  hashWithSalt _salt CanaryCodeInput' {..} =
    _salt `Prelude.hashWithSalt` s3Bucket
      `Prelude.hashWithSalt` s3Key
      `Prelude.hashWithSalt` s3Version
      `Prelude.hashWithSalt` zipFile
      `Prelude.hashWithSalt` handler

instance Prelude.NFData CanaryCodeInput where
  rnf CanaryCodeInput' {..} =
    Prelude.rnf s3Bucket
      `Prelude.seq` Prelude.rnf s3Key
      `Prelude.seq` Prelude.rnf s3Version
      `Prelude.seq` Prelude.rnf zipFile
      `Prelude.seq` Prelude.rnf handler

instance Data.ToJSON CanaryCodeInput where
  toJSON CanaryCodeInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("S3Bucket" Data..=) Prelude.<$> s3Bucket,
            ("S3Key" Data..=) Prelude.<$> s3Key,
            ("S3Version" Data..=) Prelude.<$> s3Version,
            ("ZipFile" Data..=) Prelude.<$> zipFile,
            Prelude.Just ("Handler" Data..= handler)
          ]
      )
