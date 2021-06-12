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
-- Module      : Network.AWS.Firehose.Types.ElasticsearchRetryOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.ElasticsearchRetryOptions where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Configures retry behavior in case Kinesis Data Firehose is unable to
-- deliver documents to Amazon ES.
--
-- /See:/ 'newElasticsearchRetryOptions' smart constructor.
data ElasticsearchRetryOptions = ElasticsearchRetryOptions'
  { -- | After an initial failure to deliver to Amazon ES, the total amount of
    -- time during which Kinesis Data Firehose retries delivery (including the
    -- first attempt). After this time has elapsed, the failed documents are
    -- written to Amazon S3. Default value is 300 seconds (5 minutes). A value
    -- of 0 (zero) results in no retries.
    durationInSeconds :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ElasticsearchRetryOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'durationInSeconds', 'elasticsearchRetryOptions_durationInSeconds' - After an initial failure to deliver to Amazon ES, the total amount of
-- time during which Kinesis Data Firehose retries delivery (including the
-- first attempt). After this time has elapsed, the failed documents are
-- written to Amazon S3. Default value is 300 seconds (5 minutes). A value
-- of 0 (zero) results in no retries.
newElasticsearchRetryOptions ::
  ElasticsearchRetryOptions
newElasticsearchRetryOptions =
  ElasticsearchRetryOptions'
    { durationInSeconds =
        Core.Nothing
    }

-- | After an initial failure to deliver to Amazon ES, the total amount of
-- time during which Kinesis Data Firehose retries delivery (including the
-- first attempt). After this time has elapsed, the failed documents are
-- written to Amazon S3. Default value is 300 seconds (5 minutes). A value
-- of 0 (zero) results in no retries.
elasticsearchRetryOptions_durationInSeconds :: Lens.Lens' ElasticsearchRetryOptions (Core.Maybe Core.Natural)
elasticsearchRetryOptions_durationInSeconds = Lens.lens (\ElasticsearchRetryOptions' {durationInSeconds} -> durationInSeconds) (\s@ElasticsearchRetryOptions' {} a -> s {durationInSeconds = a} :: ElasticsearchRetryOptions)

instance Core.FromJSON ElasticsearchRetryOptions where
  parseJSON =
    Core.withObject
      "ElasticsearchRetryOptions"
      ( \x ->
          ElasticsearchRetryOptions'
            Core.<$> (x Core..:? "DurationInSeconds")
      )

instance Core.Hashable ElasticsearchRetryOptions

instance Core.NFData ElasticsearchRetryOptions

instance Core.ToJSON ElasticsearchRetryOptions where
  toJSON ElasticsearchRetryOptions' {..} =
    Core.object
      ( Core.catMaybes
          [ ("DurationInSeconds" Core..=)
              Core.<$> durationInSeconds
          ]
      )
