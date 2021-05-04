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
-- Module      : Network.AWS.ElasticSearch.Types.EBSOptionsStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.EBSOptionsStatus where

import Network.AWS.ElasticSearch.Types.EBSOptions
import Network.AWS.ElasticSearch.Types.OptionStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Status of the EBS options for the specified Elasticsearch domain.
--
-- /See:/ 'newEBSOptionsStatus' smart constructor.
data EBSOptionsStatus = EBSOptionsStatus'
  { -- | Specifies the EBS options for the specified Elasticsearch domain.
    options :: EBSOptions,
    -- | Specifies the status of the EBS options for the specified Elasticsearch
    -- domain.
    status :: OptionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EBSOptionsStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'options', 'eBSOptionsStatus_options' - Specifies the EBS options for the specified Elasticsearch domain.
--
-- 'status', 'eBSOptionsStatus_status' - Specifies the status of the EBS options for the specified Elasticsearch
-- domain.
newEBSOptionsStatus ::
  -- | 'options'
  EBSOptions ->
  -- | 'status'
  OptionStatus ->
  EBSOptionsStatus
newEBSOptionsStatus pOptions_ pStatus_ =
  EBSOptionsStatus'
    { options = pOptions_,
      status = pStatus_
    }

-- | Specifies the EBS options for the specified Elasticsearch domain.
eBSOptionsStatus_options :: Lens.Lens' EBSOptionsStatus EBSOptions
eBSOptionsStatus_options = Lens.lens (\EBSOptionsStatus' {options} -> options) (\s@EBSOptionsStatus' {} a -> s {options = a} :: EBSOptionsStatus)

-- | Specifies the status of the EBS options for the specified Elasticsearch
-- domain.
eBSOptionsStatus_status :: Lens.Lens' EBSOptionsStatus OptionStatus
eBSOptionsStatus_status = Lens.lens (\EBSOptionsStatus' {status} -> status) (\s@EBSOptionsStatus' {} a -> s {status = a} :: EBSOptionsStatus)

instance Prelude.FromJSON EBSOptionsStatus where
  parseJSON =
    Prelude.withObject
      "EBSOptionsStatus"
      ( \x ->
          EBSOptionsStatus'
            Prelude.<$> (x Prelude..: "Options")
            Prelude.<*> (x Prelude..: "Status")
      )

instance Prelude.Hashable EBSOptionsStatus

instance Prelude.NFData EBSOptionsStatus
