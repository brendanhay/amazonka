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
-- Module      : Amazonka.IoT.Types.PresignedUrlConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.PresignedUrlConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Configuration for pre-signed S3 URLs.
--
-- /See:/ 'newPresignedUrlConfig' smart constructor.
data PresignedUrlConfig = PresignedUrlConfig'
  { -- | How long (in seconds) pre-signed URLs are valid. Valid values are 60 -
    -- 3600, the default value is 3600 seconds. Pre-signed URLs are generated
    -- when Jobs receives an MQTT request for the job document.
    expiresInSec :: Prelude.Maybe Prelude.Natural,
    -- | The ARN of an IAM role that grants permission to download files from the
    -- S3 bucket where the job data\/updates are stored. The role must also
    -- grant permission for IoT to download the files.
    --
    -- For information about addressing the confused deputy problem, see
    -- <https://docs.aws.amazon.com/iot/latest/developerguide/cross-service-confused-deputy-prevention.html cross-service confused deputy prevention>
    -- in the /Amazon Web Services IoT Core developer guide/.
    roleArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PresignedUrlConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expiresInSec', 'presignedUrlConfig_expiresInSec' - How long (in seconds) pre-signed URLs are valid. Valid values are 60 -
-- 3600, the default value is 3600 seconds. Pre-signed URLs are generated
-- when Jobs receives an MQTT request for the job document.
--
-- 'roleArn', 'presignedUrlConfig_roleArn' - The ARN of an IAM role that grants permission to download files from the
-- S3 bucket where the job data\/updates are stored. The role must also
-- grant permission for IoT to download the files.
--
-- For information about addressing the confused deputy problem, see
-- <https://docs.aws.amazon.com/iot/latest/developerguide/cross-service-confused-deputy-prevention.html cross-service confused deputy prevention>
-- in the /Amazon Web Services IoT Core developer guide/.
newPresignedUrlConfig ::
  PresignedUrlConfig
newPresignedUrlConfig =
  PresignedUrlConfig'
    { expiresInSec = Prelude.Nothing,
      roleArn = Prelude.Nothing
    }

-- | How long (in seconds) pre-signed URLs are valid. Valid values are 60 -
-- 3600, the default value is 3600 seconds. Pre-signed URLs are generated
-- when Jobs receives an MQTT request for the job document.
presignedUrlConfig_expiresInSec :: Lens.Lens' PresignedUrlConfig (Prelude.Maybe Prelude.Natural)
presignedUrlConfig_expiresInSec = Lens.lens (\PresignedUrlConfig' {expiresInSec} -> expiresInSec) (\s@PresignedUrlConfig' {} a -> s {expiresInSec = a} :: PresignedUrlConfig)

-- | The ARN of an IAM role that grants permission to download files from the
-- S3 bucket where the job data\/updates are stored. The role must also
-- grant permission for IoT to download the files.
--
-- For information about addressing the confused deputy problem, see
-- <https://docs.aws.amazon.com/iot/latest/developerguide/cross-service-confused-deputy-prevention.html cross-service confused deputy prevention>
-- in the /Amazon Web Services IoT Core developer guide/.
presignedUrlConfig_roleArn :: Lens.Lens' PresignedUrlConfig (Prelude.Maybe Prelude.Text)
presignedUrlConfig_roleArn = Lens.lens (\PresignedUrlConfig' {roleArn} -> roleArn) (\s@PresignedUrlConfig' {} a -> s {roleArn = a} :: PresignedUrlConfig)

instance Data.FromJSON PresignedUrlConfig where
  parseJSON =
    Data.withObject
      "PresignedUrlConfig"
      ( \x ->
          PresignedUrlConfig'
            Prelude.<$> (x Data..:? "expiresInSec")
            Prelude.<*> (x Data..:? "roleArn")
      )

instance Prelude.Hashable PresignedUrlConfig where
  hashWithSalt _salt PresignedUrlConfig' {..} =
    _salt
      `Prelude.hashWithSalt` expiresInSec
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData PresignedUrlConfig where
  rnf PresignedUrlConfig' {..} =
    Prelude.rnf expiresInSec
      `Prelude.seq` Prelude.rnf roleArn

instance Data.ToJSON PresignedUrlConfig where
  toJSON PresignedUrlConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("expiresInSec" Data..=) Prelude.<$> expiresInSec,
            ("roleArn" Data..=) Prelude.<$> roleArn
          ]
      )
