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
-- Module      : Amazonka.OpenSearch.Types.VersionStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.VersionStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types.OptionStatus
import qualified Amazonka.Prelude as Prelude

-- | The status of the the OpenSearch or Elasticsearch version options for
-- the specified Amazon OpenSearch Service domain.
--
-- /See:/ 'newVersionStatus' smart constructor.
data VersionStatus = VersionStatus'
  { -- | The OpenSearch or Elasticsearch version for the specified domain.
    options :: Prelude.Text,
    -- | The status of the version options for the specified domain.
    status :: OptionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VersionStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'options', 'versionStatus_options' - The OpenSearch or Elasticsearch version for the specified domain.
--
-- 'status', 'versionStatus_status' - The status of the version options for the specified domain.
newVersionStatus ::
  -- | 'options'
  Prelude.Text ->
  -- | 'status'
  OptionStatus ->
  VersionStatus
newVersionStatus pOptions_ pStatus_ =
  VersionStatus'
    { options = pOptions_,
      status = pStatus_
    }

-- | The OpenSearch or Elasticsearch version for the specified domain.
versionStatus_options :: Lens.Lens' VersionStatus Prelude.Text
versionStatus_options = Lens.lens (\VersionStatus' {options} -> options) (\s@VersionStatus' {} a -> s {options = a} :: VersionStatus)

-- | The status of the version options for the specified domain.
versionStatus_status :: Lens.Lens' VersionStatus OptionStatus
versionStatus_status = Lens.lens (\VersionStatus' {status} -> status) (\s@VersionStatus' {} a -> s {status = a} :: VersionStatus)

instance Data.FromJSON VersionStatus where
  parseJSON =
    Data.withObject
      "VersionStatus"
      ( \x ->
          VersionStatus'
            Prelude.<$> (x Data..: "Options")
            Prelude.<*> (x Data..: "Status")
      )

instance Prelude.Hashable VersionStatus where
  hashWithSalt _salt VersionStatus' {..} =
    _salt
      `Prelude.hashWithSalt` options
      `Prelude.hashWithSalt` status

instance Prelude.NFData VersionStatus where
  rnf VersionStatus' {..} =
    Prelude.rnf options
      `Prelude.seq` Prelude.rnf status
