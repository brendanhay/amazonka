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
-- Module      : Amazonka.DevOpsGuru.Types.EventSourcesConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.EventSourcesConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DevOpsGuru.Types.AmazonCodeGuruProfilerIntegration
import qualified Amazonka.Prelude as Prelude

-- | Information about the integration of DevOps Guru as consumer with
-- another AWS service, such as AWS CodeGuru Profiler via EventBridge.
--
-- /See:/ 'newEventSourcesConfig' smart constructor.
data EventSourcesConfig = EventSourcesConfig'
  { -- | Information about whether DevOps Guru is configured to consume
    -- recommendations which are generated from AWS CodeGuru Profiler.
    amazonCodeGuruProfiler :: Prelude.Maybe AmazonCodeGuruProfilerIntegration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EventSourcesConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'amazonCodeGuruProfiler', 'eventSourcesConfig_amazonCodeGuruProfiler' - Information about whether DevOps Guru is configured to consume
-- recommendations which are generated from AWS CodeGuru Profiler.
newEventSourcesConfig ::
  EventSourcesConfig
newEventSourcesConfig =
  EventSourcesConfig'
    { amazonCodeGuruProfiler =
        Prelude.Nothing
    }

-- | Information about whether DevOps Guru is configured to consume
-- recommendations which are generated from AWS CodeGuru Profiler.
eventSourcesConfig_amazonCodeGuruProfiler :: Lens.Lens' EventSourcesConfig (Prelude.Maybe AmazonCodeGuruProfilerIntegration)
eventSourcesConfig_amazonCodeGuruProfiler = Lens.lens (\EventSourcesConfig' {amazonCodeGuruProfiler} -> amazonCodeGuruProfiler) (\s@EventSourcesConfig' {} a -> s {amazonCodeGuruProfiler = a} :: EventSourcesConfig)

instance Data.FromJSON EventSourcesConfig where
  parseJSON =
    Data.withObject
      "EventSourcesConfig"
      ( \x ->
          EventSourcesConfig'
            Prelude.<$> (x Data..:? "AmazonCodeGuruProfiler")
      )

instance Prelude.Hashable EventSourcesConfig where
  hashWithSalt _salt EventSourcesConfig' {..} =
    _salt `Prelude.hashWithSalt` amazonCodeGuruProfiler

instance Prelude.NFData EventSourcesConfig where
  rnf EventSourcesConfig' {..} =
    Prelude.rnf amazonCodeGuruProfiler

instance Data.ToJSON EventSourcesConfig where
  toJSON EventSourcesConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AmazonCodeGuruProfiler" Data..=)
              Prelude.<$> amazonCodeGuruProfiler
          ]
      )
