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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.CloudWatchLoggingOptionUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.CloudWatchLoggingOptionUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the Amazon CloudWatch logging option updates.
--
-- /See:/ 'newCloudWatchLoggingOptionUpdate' smart constructor.
data CloudWatchLoggingOptionUpdate = CloudWatchLoggingOptionUpdate'
  { -- | The Amazon Resource Name (ARN) of the CloudWatch log to receive
    -- application messages.
    logStreamARNUpdate :: Prelude.Maybe Prelude.Text,
    -- | The ID of the CloudWatch logging option to update
    cloudWatchLoggingOptionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CloudWatchLoggingOptionUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logStreamARNUpdate', 'cloudWatchLoggingOptionUpdate_logStreamARNUpdate' - The Amazon Resource Name (ARN) of the CloudWatch log to receive
-- application messages.
--
-- 'cloudWatchLoggingOptionId', 'cloudWatchLoggingOptionUpdate_cloudWatchLoggingOptionId' - The ID of the CloudWatch logging option to update
newCloudWatchLoggingOptionUpdate ::
  -- | 'cloudWatchLoggingOptionId'
  Prelude.Text ->
  CloudWatchLoggingOptionUpdate
newCloudWatchLoggingOptionUpdate
  pCloudWatchLoggingOptionId_ =
    CloudWatchLoggingOptionUpdate'
      { logStreamARNUpdate =
          Prelude.Nothing,
        cloudWatchLoggingOptionId =
          pCloudWatchLoggingOptionId_
      }

-- | The Amazon Resource Name (ARN) of the CloudWatch log to receive
-- application messages.
cloudWatchLoggingOptionUpdate_logStreamARNUpdate :: Lens.Lens' CloudWatchLoggingOptionUpdate (Prelude.Maybe Prelude.Text)
cloudWatchLoggingOptionUpdate_logStreamARNUpdate = Lens.lens (\CloudWatchLoggingOptionUpdate' {logStreamARNUpdate} -> logStreamARNUpdate) (\s@CloudWatchLoggingOptionUpdate' {} a -> s {logStreamARNUpdate = a} :: CloudWatchLoggingOptionUpdate)

-- | The ID of the CloudWatch logging option to update
cloudWatchLoggingOptionUpdate_cloudWatchLoggingOptionId :: Lens.Lens' CloudWatchLoggingOptionUpdate Prelude.Text
cloudWatchLoggingOptionUpdate_cloudWatchLoggingOptionId = Lens.lens (\CloudWatchLoggingOptionUpdate' {cloudWatchLoggingOptionId} -> cloudWatchLoggingOptionId) (\s@CloudWatchLoggingOptionUpdate' {} a -> s {cloudWatchLoggingOptionId = a} :: CloudWatchLoggingOptionUpdate)

instance
  Prelude.Hashable
    CloudWatchLoggingOptionUpdate
  where
  hashWithSalt _salt CloudWatchLoggingOptionUpdate' {..} =
    _salt
      `Prelude.hashWithSalt` logStreamARNUpdate
      `Prelude.hashWithSalt` cloudWatchLoggingOptionId

instance Prelude.NFData CloudWatchLoggingOptionUpdate where
  rnf CloudWatchLoggingOptionUpdate' {..} =
    Prelude.rnf logStreamARNUpdate `Prelude.seq`
      Prelude.rnf cloudWatchLoggingOptionId

instance Data.ToJSON CloudWatchLoggingOptionUpdate where
  toJSON CloudWatchLoggingOptionUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LogStreamARNUpdate" Data..=)
              Prelude.<$> logStreamARNUpdate,
            Prelude.Just
              ( "CloudWatchLoggingOptionId"
                  Data..= cloudWatchLoggingOptionId
              )
          ]
      )
