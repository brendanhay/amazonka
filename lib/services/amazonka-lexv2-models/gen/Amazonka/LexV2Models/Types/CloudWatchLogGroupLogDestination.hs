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
-- Module      : Amazonka.LexV2Models.Types.CloudWatchLogGroupLogDestination
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.CloudWatchLogGroupLogDestination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The Amazon CloudWatch Logs log group where the text and metadata logs
-- are delivered. The log group must exist before you enable logging.
--
-- /See:/ 'newCloudWatchLogGroupLogDestination' smart constructor.
data CloudWatchLogGroupLogDestination = CloudWatchLogGroupLogDestination'
  { -- | The Amazon Resource Name (ARN) of the log group where text and metadata
    -- logs are delivered.
    cloudWatchLogGroupArn :: Prelude.Text,
    -- | The prefix of the log stream name within the log group that you
    -- specified
    logPrefix :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CloudWatchLogGroupLogDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudWatchLogGroupArn', 'cloudWatchLogGroupLogDestination_cloudWatchLogGroupArn' - The Amazon Resource Name (ARN) of the log group where text and metadata
-- logs are delivered.
--
-- 'logPrefix', 'cloudWatchLogGroupLogDestination_logPrefix' - The prefix of the log stream name within the log group that you
-- specified
newCloudWatchLogGroupLogDestination ::
  -- | 'cloudWatchLogGroupArn'
  Prelude.Text ->
  -- | 'logPrefix'
  Prelude.Text ->
  CloudWatchLogGroupLogDestination
newCloudWatchLogGroupLogDestination
  pCloudWatchLogGroupArn_
  pLogPrefix_ =
    CloudWatchLogGroupLogDestination'
      { cloudWatchLogGroupArn =
          pCloudWatchLogGroupArn_,
        logPrefix = pLogPrefix_
      }

-- | The Amazon Resource Name (ARN) of the log group where text and metadata
-- logs are delivered.
cloudWatchLogGroupLogDestination_cloudWatchLogGroupArn :: Lens.Lens' CloudWatchLogGroupLogDestination Prelude.Text
cloudWatchLogGroupLogDestination_cloudWatchLogGroupArn = Lens.lens (\CloudWatchLogGroupLogDestination' {cloudWatchLogGroupArn} -> cloudWatchLogGroupArn) (\s@CloudWatchLogGroupLogDestination' {} a -> s {cloudWatchLogGroupArn = a} :: CloudWatchLogGroupLogDestination)

-- | The prefix of the log stream name within the log group that you
-- specified
cloudWatchLogGroupLogDestination_logPrefix :: Lens.Lens' CloudWatchLogGroupLogDestination Prelude.Text
cloudWatchLogGroupLogDestination_logPrefix = Lens.lens (\CloudWatchLogGroupLogDestination' {logPrefix} -> logPrefix) (\s@CloudWatchLogGroupLogDestination' {} a -> s {logPrefix = a} :: CloudWatchLogGroupLogDestination)

instance
  Data.FromJSON
    CloudWatchLogGroupLogDestination
  where
  parseJSON =
    Data.withObject
      "CloudWatchLogGroupLogDestination"
      ( \x ->
          CloudWatchLogGroupLogDestination'
            Prelude.<$> (x Data..: "cloudWatchLogGroupArn")
            Prelude.<*> (x Data..: "logPrefix")
      )

instance
  Prelude.Hashable
    CloudWatchLogGroupLogDestination
  where
  hashWithSalt
    _salt
    CloudWatchLogGroupLogDestination' {..} =
      _salt
        `Prelude.hashWithSalt` cloudWatchLogGroupArn
        `Prelude.hashWithSalt` logPrefix

instance
  Prelude.NFData
    CloudWatchLogGroupLogDestination
  where
  rnf CloudWatchLogGroupLogDestination' {..} =
    Prelude.rnf cloudWatchLogGroupArn
      `Prelude.seq` Prelude.rnf logPrefix

instance Data.ToJSON CloudWatchLogGroupLogDestination where
  toJSON CloudWatchLogGroupLogDestination' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "cloudWatchLogGroupArn"
                  Data..= cloudWatchLogGroupArn
              ),
            Prelude.Just ("logPrefix" Data..= logPrefix)
          ]
      )
