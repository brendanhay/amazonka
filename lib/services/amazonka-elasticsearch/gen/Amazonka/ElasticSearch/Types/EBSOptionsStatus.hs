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
-- Module      : Amazonka.ElasticSearch.Types.EBSOptionsStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticSearch.Types.EBSOptionsStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticSearch.Types.EBSOptions
import Amazonka.ElasticSearch.Types.OptionStatus
import qualified Amazonka.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON EBSOptionsStatus where
  parseJSON =
    Data.withObject
      "EBSOptionsStatus"
      ( \x ->
          EBSOptionsStatus'
            Prelude.<$> (x Data..: "Options")
            Prelude.<*> (x Data..: "Status")
      )

instance Prelude.Hashable EBSOptionsStatus where
  hashWithSalt _salt EBSOptionsStatus' {..} =
    _salt `Prelude.hashWithSalt` options
      `Prelude.hashWithSalt` status

instance Prelude.NFData EBSOptionsStatus where
  rnf EBSOptionsStatus' {..} =
    Prelude.rnf options
      `Prelude.seq` Prelude.rnf status
