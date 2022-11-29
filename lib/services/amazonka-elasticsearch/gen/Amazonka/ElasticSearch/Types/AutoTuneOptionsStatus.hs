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
-- Module      : Amazonka.ElasticSearch.Types.AutoTuneOptionsStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticSearch.Types.AutoTuneOptionsStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ElasticSearch.Types.AutoTuneOptions
import Amazonka.ElasticSearch.Types.AutoTuneStatus
import qualified Amazonka.Prelude as Prelude

-- | Specifies the status of Auto-Tune options for the specified
-- Elasticsearch domain.
--
-- /See:/ 'newAutoTuneOptionsStatus' smart constructor.
data AutoTuneOptionsStatus = AutoTuneOptionsStatus'
  { -- | Specifies Status of the Auto-Tune options for the specified
    -- Elasticsearch domain.
    status :: Prelude.Maybe AutoTuneStatus,
    -- | Specifies Auto-Tune options for the specified Elasticsearch domain.
    options :: Prelude.Maybe AutoTuneOptions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoTuneOptionsStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'autoTuneOptionsStatus_status' - Specifies Status of the Auto-Tune options for the specified
-- Elasticsearch domain.
--
-- 'options', 'autoTuneOptionsStatus_options' - Specifies Auto-Tune options for the specified Elasticsearch domain.
newAutoTuneOptionsStatus ::
  AutoTuneOptionsStatus
newAutoTuneOptionsStatus =
  AutoTuneOptionsStatus'
    { status = Prelude.Nothing,
      options = Prelude.Nothing
    }

-- | Specifies Status of the Auto-Tune options for the specified
-- Elasticsearch domain.
autoTuneOptionsStatus_status :: Lens.Lens' AutoTuneOptionsStatus (Prelude.Maybe AutoTuneStatus)
autoTuneOptionsStatus_status = Lens.lens (\AutoTuneOptionsStatus' {status} -> status) (\s@AutoTuneOptionsStatus' {} a -> s {status = a} :: AutoTuneOptionsStatus)

-- | Specifies Auto-Tune options for the specified Elasticsearch domain.
autoTuneOptionsStatus_options :: Lens.Lens' AutoTuneOptionsStatus (Prelude.Maybe AutoTuneOptions)
autoTuneOptionsStatus_options = Lens.lens (\AutoTuneOptionsStatus' {options} -> options) (\s@AutoTuneOptionsStatus' {} a -> s {options = a} :: AutoTuneOptionsStatus)

instance Core.FromJSON AutoTuneOptionsStatus where
  parseJSON =
    Core.withObject
      "AutoTuneOptionsStatus"
      ( \x ->
          AutoTuneOptionsStatus'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "Options")
      )

instance Prelude.Hashable AutoTuneOptionsStatus where
  hashWithSalt _salt AutoTuneOptionsStatus' {..} =
    _salt `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` options

instance Prelude.NFData AutoTuneOptionsStatus where
  rnf AutoTuneOptionsStatus' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf options
