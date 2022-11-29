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
-- Module      : Amazonka.LexModels.Types.LogSettingsResponse
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexModels.Types.LogSettingsResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LexModels.Types.Destination
import Amazonka.LexModels.Types.LogType
import qualified Amazonka.Prelude as Prelude

-- | The settings for conversation logs.
--
-- /See:/ 'newLogSettingsResponse' smart constructor.
data LogSettingsResponse = LogSettingsResponse'
  { -- | The destination where logs are delivered.
    destination :: Prelude.Maybe Destination,
    -- | The type of logging that is enabled.
    logType :: Prelude.Maybe LogType,
    -- | The resource prefix is the first part of the S3 object key within the S3
    -- bucket that you specified to contain audio logs. For CloudWatch Logs it
    -- is the prefix of the log stream name within the log group that you
    -- specified.
    resourcePrefix :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the key used to encrypt audio logs in
    -- an S3 bucket.
    kmsKeyArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the CloudWatch Logs log group or S3
    -- bucket where the logs are delivered.
    resourceArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LogSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destination', 'logSettingsResponse_destination' - The destination where logs are delivered.
--
-- 'logType', 'logSettingsResponse_logType' - The type of logging that is enabled.
--
-- 'resourcePrefix', 'logSettingsResponse_resourcePrefix' - The resource prefix is the first part of the S3 object key within the S3
-- bucket that you specified to contain audio logs. For CloudWatch Logs it
-- is the prefix of the log stream name within the log group that you
-- specified.
--
-- 'kmsKeyArn', 'logSettingsResponse_kmsKeyArn' - The Amazon Resource Name (ARN) of the key used to encrypt audio logs in
-- an S3 bucket.
--
-- 'resourceArn', 'logSettingsResponse_resourceArn' - The Amazon Resource Name (ARN) of the CloudWatch Logs log group or S3
-- bucket where the logs are delivered.
newLogSettingsResponse ::
  LogSettingsResponse
newLogSettingsResponse =
  LogSettingsResponse'
    { destination = Prelude.Nothing,
      logType = Prelude.Nothing,
      resourcePrefix = Prelude.Nothing,
      kmsKeyArn = Prelude.Nothing,
      resourceArn = Prelude.Nothing
    }

-- | The destination where logs are delivered.
logSettingsResponse_destination :: Lens.Lens' LogSettingsResponse (Prelude.Maybe Destination)
logSettingsResponse_destination = Lens.lens (\LogSettingsResponse' {destination} -> destination) (\s@LogSettingsResponse' {} a -> s {destination = a} :: LogSettingsResponse)

-- | The type of logging that is enabled.
logSettingsResponse_logType :: Lens.Lens' LogSettingsResponse (Prelude.Maybe LogType)
logSettingsResponse_logType = Lens.lens (\LogSettingsResponse' {logType} -> logType) (\s@LogSettingsResponse' {} a -> s {logType = a} :: LogSettingsResponse)

-- | The resource prefix is the first part of the S3 object key within the S3
-- bucket that you specified to contain audio logs. For CloudWatch Logs it
-- is the prefix of the log stream name within the log group that you
-- specified.
logSettingsResponse_resourcePrefix :: Lens.Lens' LogSettingsResponse (Prelude.Maybe Prelude.Text)
logSettingsResponse_resourcePrefix = Lens.lens (\LogSettingsResponse' {resourcePrefix} -> resourcePrefix) (\s@LogSettingsResponse' {} a -> s {resourcePrefix = a} :: LogSettingsResponse)

-- | The Amazon Resource Name (ARN) of the key used to encrypt audio logs in
-- an S3 bucket.
logSettingsResponse_kmsKeyArn :: Lens.Lens' LogSettingsResponse (Prelude.Maybe Prelude.Text)
logSettingsResponse_kmsKeyArn = Lens.lens (\LogSettingsResponse' {kmsKeyArn} -> kmsKeyArn) (\s@LogSettingsResponse' {} a -> s {kmsKeyArn = a} :: LogSettingsResponse)

-- | The Amazon Resource Name (ARN) of the CloudWatch Logs log group or S3
-- bucket where the logs are delivered.
logSettingsResponse_resourceArn :: Lens.Lens' LogSettingsResponse (Prelude.Maybe Prelude.Text)
logSettingsResponse_resourceArn = Lens.lens (\LogSettingsResponse' {resourceArn} -> resourceArn) (\s@LogSettingsResponse' {} a -> s {resourceArn = a} :: LogSettingsResponse)

instance Core.FromJSON LogSettingsResponse where
  parseJSON =
    Core.withObject
      "LogSettingsResponse"
      ( \x ->
          LogSettingsResponse'
            Prelude.<$> (x Core..:? "destination")
            Prelude.<*> (x Core..:? "logType")
            Prelude.<*> (x Core..:? "resourcePrefix")
            Prelude.<*> (x Core..:? "kmsKeyArn")
            Prelude.<*> (x Core..:? "resourceArn")
      )

instance Prelude.Hashable LogSettingsResponse where
  hashWithSalt _salt LogSettingsResponse' {..} =
    _salt `Prelude.hashWithSalt` destination
      `Prelude.hashWithSalt` logType
      `Prelude.hashWithSalt` resourcePrefix
      `Prelude.hashWithSalt` kmsKeyArn
      `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData LogSettingsResponse where
  rnf LogSettingsResponse' {..} =
    Prelude.rnf destination
      `Prelude.seq` Prelude.rnf logType
      `Prelude.seq` Prelude.rnf resourcePrefix
      `Prelude.seq` Prelude.rnf kmsKeyArn
      `Prelude.seq` Prelude.rnf resourceArn
