{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.ElasticSearch.Types.VPCDerivedInfoStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.VPCDerivedInfoStatus where

import Network.AWS.ElasticSearch.Types.OptionStatus
import Network.AWS.ElasticSearch.Types.VPCDerivedInfo
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON VPCDerivedInfoStatus where
  parseJSON =
    Prelude.withObject
      "VPCDerivedInfoStatus"
      ( \x ->
          VPCDerivedInfoStatus'
            Prelude.<$> (x Prelude..: "Options")
            Prelude.<*> (x Prelude..: "Status")
      )

instance Prelude.Hashable VPCDerivedInfoStatus

instance Prelude.NFData VPCDerivedInfoStatus
