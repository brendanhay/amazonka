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
-- Module      : Amazonka.Evidently.Types.CloudWatchLogsDestination
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Evidently.Types.CloudWatchLogsDestination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure containing the CloudWatch Logs log group where the project
-- stores evaluation events.
--
-- /See:/ 'newCloudWatchLogsDestination' smart constructor.
data CloudWatchLogsDestination = CloudWatchLogsDestination'
  { -- | The name of the log group where the project stores evaluation events.
    logGroup :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CloudWatchLogsDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logGroup', 'cloudWatchLogsDestination_logGroup' - The name of the log group where the project stores evaluation events.
newCloudWatchLogsDestination ::
  CloudWatchLogsDestination
newCloudWatchLogsDestination =
  CloudWatchLogsDestination'
    { logGroup =
        Prelude.Nothing
    }

-- | The name of the log group where the project stores evaluation events.
cloudWatchLogsDestination_logGroup :: Lens.Lens' CloudWatchLogsDestination (Prelude.Maybe Prelude.Text)
cloudWatchLogsDestination_logGroup = Lens.lens (\CloudWatchLogsDestination' {logGroup} -> logGroup) (\s@CloudWatchLogsDestination' {} a -> s {logGroup = a} :: CloudWatchLogsDestination)

instance Data.FromJSON CloudWatchLogsDestination where
  parseJSON =
    Data.withObject
      "CloudWatchLogsDestination"
      ( \x ->
          CloudWatchLogsDestination'
            Prelude.<$> (x Data..:? "logGroup")
      )

instance Prelude.Hashable CloudWatchLogsDestination where
  hashWithSalt _salt CloudWatchLogsDestination' {..} =
    _salt `Prelude.hashWithSalt` logGroup

instance Prelude.NFData CloudWatchLogsDestination where
  rnf CloudWatchLogsDestination' {..} =
    Prelude.rnf logGroup
