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
-- Module      : Network.AWS.OpenSearch.Types.VersionStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpenSearch.Types.VersionStatus where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpenSearch.Types.OptionStatus
import qualified Network.AWS.Prelude as Prelude

-- | The status of the OpenSearch version options for the specified
-- OpenSearch domain.
--
-- /See:/ 'newVersionStatus' smart constructor.
data VersionStatus = VersionStatus'
  { -- | The OpenSearch version for the specified OpenSearch domain.
    options :: Prelude.Text,
    -- | The status of the OpenSearch version options for the specified
    -- OpenSearch domain.
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
-- 'options', 'versionStatus_options' - The OpenSearch version for the specified OpenSearch domain.
--
-- 'status', 'versionStatus_status' - The status of the OpenSearch version options for the specified
-- OpenSearch domain.
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

-- | The OpenSearch version for the specified OpenSearch domain.
versionStatus_options :: Lens.Lens' VersionStatus Prelude.Text
versionStatus_options = Lens.lens (\VersionStatus' {options} -> options) (\s@VersionStatus' {} a -> s {options = a} :: VersionStatus)

-- | The status of the OpenSearch version options for the specified
-- OpenSearch domain.
versionStatus_status :: Lens.Lens' VersionStatus OptionStatus
versionStatus_status = Lens.lens (\VersionStatus' {status} -> status) (\s@VersionStatus' {} a -> s {status = a} :: VersionStatus)

instance Core.FromJSON VersionStatus where
  parseJSON =
    Core.withObject
      "VersionStatus"
      ( \x ->
          VersionStatus'
            Prelude.<$> (x Core..: "Options")
            Prelude.<*> (x Core..: "Status")
      )

instance Prelude.Hashable VersionStatus

instance Prelude.NFData VersionStatus
