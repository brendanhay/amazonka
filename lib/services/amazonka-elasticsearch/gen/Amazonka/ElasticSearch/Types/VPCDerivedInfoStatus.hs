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
-- Module      : Amazonka.ElasticSearch.Types.VPCDerivedInfoStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticSearch.Types.VPCDerivedInfoStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticSearch.Types.OptionStatus
import Amazonka.ElasticSearch.Types.VPCDerivedInfo
import qualified Amazonka.Prelude as Prelude

-- | Status of the VPC options for the specified Elasticsearch domain.
--
-- /See:/ 'newVPCDerivedInfoStatus' smart constructor.
data VPCDerivedInfoStatus = VPCDerivedInfoStatus'
  { -- | Specifies the VPC options for the specified Elasticsearch domain.
    options :: VPCDerivedInfo,
    -- | Specifies the status of the VPC options for the specified Elasticsearch
    -- domain.
    status :: OptionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VPCDerivedInfoStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'options', 'vPCDerivedInfoStatus_options' - Specifies the VPC options for the specified Elasticsearch domain.
--
-- 'status', 'vPCDerivedInfoStatus_status' - Specifies the status of the VPC options for the specified Elasticsearch
-- domain.
newVPCDerivedInfoStatus ::
  -- | 'options'
  VPCDerivedInfo ->
  -- | 'status'
  OptionStatus ->
  VPCDerivedInfoStatus
newVPCDerivedInfoStatus pOptions_ pStatus_ =
  VPCDerivedInfoStatus'
    { options = pOptions_,
      status = pStatus_
    }

-- | Specifies the VPC options for the specified Elasticsearch domain.
vPCDerivedInfoStatus_options :: Lens.Lens' VPCDerivedInfoStatus VPCDerivedInfo
vPCDerivedInfoStatus_options = Lens.lens (\VPCDerivedInfoStatus' {options} -> options) (\s@VPCDerivedInfoStatus' {} a -> s {options = a} :: VPCDerivedInfoStatus)

-- | Specifies the status of the VPC options for the specified Elasticsearch
-- domain.
vPCDerivedInfoStatus_status :: Lens.Lens' VPCDerivedInfoStatus OptionStatus
vPCDerivedInfoStatus_status = Lens.lens (\VPCDerivedInfoStatus' {status} -> status) (\s@VPCDerivedInfoStatus' {} a -> s {status = a} :: VPCDerivedInfoStatus)

instance Data.FromJSON VPCDerivedInfoStatus where
  parseJSON =
    Data.withObject
      "VPCDerivedInfoStatus"
      ( \x ->
          VPCDerivedInfoStatus'
            Prelude.<$> (x Data..: "Options")
            Prelude.<*> (x Data..: "Status")
      )

instance Prelude.Hashable VPCDerivedInfoStatus where
  hashWithSalt _salt VPCDerivedInfoStatus' {..} =
    _salt
      `Prelude.hashWithSalt` options
      `Prelude.hashWithSalt` status

instance Prelude.NFData VPCDerivedInfoStatus where
  rnf VPCDerivedInfoStatus' {..} =
    Prelude.rnf options
      `Prelude.seq` Prelude.rnf status
