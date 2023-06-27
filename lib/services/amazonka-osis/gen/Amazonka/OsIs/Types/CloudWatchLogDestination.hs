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
-- Module      : Amazonka.OsIs.Types.CloudWatchLogDestination
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OsIs.Types.CloudWatchLogDestination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The destination for OpenSearch Ingestion logs sent to Amazon CloudWatch.
--
-- /See:/ 'newCloudWatchLogDestination' smart constructor.
data CloudWatchLogDestination = CloudWatchLogDestination'
  { -- | The name of the CloudWatch Logs group to send pipeline logs to. You can
    -- specify an existing log group or create a new one. For example,
    -- @\/aws\/OpenSearchService\/IngestionService\/my-pipeline@.
    logGroup :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CloudWatchLogDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logGroup', 'cloudWatchLogDestination_logGroup' - The name of the CloudWatch Logs group to send pipeline logs to. You can
-- specify an existing log group or create a new one. For example,
-- @\/aws\/OpenSearchService\/IngestionService\/my-pipeline@.
newCloudWatchLogDestination ::
  -- | 'logGroup'
  Prelude.Text ->
  CloudWatchLogDestination
newCloudWatchLogDestination pLogGroup_ =
  CloudWatchLogDestination' {logGroup = pLogGroup_}

-- | The name of the CloudWatch Logs group to send pipeline logs to. You can
-- specify an existing log group or create a new one. For example,
-- @\/aws\/OpenSearchService\/IngestionService\/my-pipeline@.
cloudWatchLogDestination_logGroup :: Lens.Lens' CloudWatchLogDestination Prelude.Text
cloudWatchLogDestination_logGroup = Lens.lens (\CloudWatchLogDestination' {logGroup} -> logGroup) (\s@CloudWatchLogDestination' {} a -> s {logGroup = a} :: CloudWatchLogDestination)

instance Data.FromJSON CloudWatchLogDestination where
  parseJSON =
    Data.withObject
      "CloudWatchLogDestination"
      ( \x ->
          CloudWatchLogDestination'
            Prelude.<$> (x Data..: "LogGroup")
      )

instance Prelude.Hashable CloudWatchLogDestination where
  hashWithSalt _salt CloudWatchLogDestination' {..} =
    _salt `Prelude.hashWithSalt` logGroup

instance Prelude.NFData CloudWatchLogDestination where
  rnf CloudWatchLogDestination' {..} =
    Prelude.rnf logGroup

instance Data.ToJSON CloudWatchLogDestination where
  toJSON CloudWatchLogDestination' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("LogGroup" Data..= logGroup)]
      )
