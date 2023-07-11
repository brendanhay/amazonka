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
-- Module      : Amazonka.DevOpsGuru.Types.AmazonCodeGuruProfilerIntegration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.AmazonCodeGuruProfilerIntegration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DevOpsGuru.Types.EventSourceOptInStatus
import qualified Amazonka.Prelude as Prelude

-- | Information about your account\'s integration with Amazon CodeGuru
-- Profiler. This returns whether DevOps Guru is configured to consume
-- recommendations generated from Amazon CodeGuru Profiler.
--
-- /See:/ 'newAmazonCodeGuruProfilerIntegration' smart constructor.
data AmazonCodeGuruProfilerIntegration = AmazonCodeGuruProfilerIntegration'
  { -- | The status of the CodeGuru Profiler integration. Specifies if DevOps
    -- Guru is enabled to consume recommendations that are generated from
    -- Amazon CodeGuru Profiler.
    status :: Prelude.Maybe EventSourceOptInStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AmazonCodeGuruProfilerIntegration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'amazonCodeGuruProfilerIntegration_status' - The status of the CodeGuru Profiler integration. Specifies if DevOps
-- Guru is enabled to consume recommendations that are generated from
-- Amazon CodeGuru Profiler.
newAmazonCodeGuruProfilerIntegration ::
  AmazonCodeGuruProfilerIntegration
newAmazonCodeGuruProfilerIntegration =
  AmazonCodeGuruProfilerIntegration'
    { status =
        Prelude.Nothing
    }

-- | The status of the CodeGuru Profiler integration. Specifies if DevOps
-- Guru is enabled to consume recommendations that are generated from
-- Amazon CodeGuru Profiler.
amazonCodeGuruProfilerIntegration_status :: Lens.Lens' AmazonCodeGuruProfilerIntegration (Prelude.Maybe EventSourceOptInStatus)
amazonCodeGuruProfilerIntegration_status = Lens.lens (\AmazonCodeGuruProfilerIntegration' {status} -> status) (\s@AmazonCodeGuruProfilerIntegration' {} a -> s {status = a} :: AmazonCodeGuruProfilerIntegration)

instance
  Data.FromJSON
    AmazonCodeGuruProfilerIntegration
  where
  parseJSON =
    Data.withObject
      "AmazonCodeGuruProfilerIntegration"
      ( \x ->
          AmazonCodeGuruProfilerIntegration'
            Prelude.<$> (x Data..:? "Status")
      )

instance
  Prelude.Hashable
    AmazonCodeGuruProfilerIntegration
  where
  hashWithSalt
    _salt
    AmazonCodeGuruProfilerIntegration' {..} =
      _salt `Prelude.hashWithSalt` status

instance
  Prelude.NFData
    AmazonCodeGuruProfilerIntegration
  where
  rnf AmazonCodeGuruProfilerIntegration' {..} =
    Prelude.rnf status

instance
  Data.ToJSON
    AmazonCodeGuruProfilerIntegration
  where
  toJSON AmazonCodeGuruProfilerIntegration' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Status" Data..=) Prelude.<$> status]
      )
