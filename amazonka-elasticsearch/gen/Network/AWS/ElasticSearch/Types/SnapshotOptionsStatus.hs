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
-- Module      : Network.AWS.ElasticSearch.Types.SnapshotOptionsStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.SnapshotOptionsStatus where

import Network.AWS.ElasticSearch.Types.OptionStatus
import Network.AWS.ElasticSearch.Types.SnapshotOptions
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Status of a daily automated snapshot.
--
-- /See:/ 'newSnapshotOptionsStatus' smart constructor.
data SnapshotOptionsStatus = SnapshotOptionsStatus'
  { -- | Specifies the daily snapshot options specified for the Elasticsearch
    -- domain.
    options :: SnapshotOptions,
    -- | Specifies the status of a daily automated snapshot.
    status :: OptionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SnapshotOptionsStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'options', 'snapshotOptionsStatus_options' - Specifies the daily snapshot options specified for the Elasticsearch
-- domain.
--
-- 'status', 'snapshotOptionsStatus_status' - Specifies the status of a daily automated snapshot.
newSnapshotOptionsStatus ::
  -- | 'options'
  SnapshotOptions ->
  -- | 'status'
  OptionStatus ->
  SnapshotOptionsStatus
newSnapshotOptionsStatus pOptions_ pStatus_ =
  SnapshotOptionsStatus'
    { options = pOptions_,
      status = pStatus_
    }

-- | Specifies the daily snapshot options specified for the Elasticsearch
-- domain.
snapshotOptionsStatus_options :: Lens.Lens' SnapshotOptionsStatus SnapshotOptions
snapshotOptionsStatus_options = Lens.lens (\SnapshotOptionsStatus' {options} -> options) (\s@SnapshotOptionsStatus' {} a -> s {options = a} :: SnapshotOptionsStatus)

-- | Specifies the status of a daily automated snapshot.
snapshotOptionsStatus_status :: Lens.Lens' SnapshotOptionsStatus OptionStatus
snapshotOptionsStatus_status = Lens.lens (\SnapshotOptionsStatus' {status} -> status) (\s@SnapshotOptionsStatus' {} a -> s {status = a} :: SnapshotOptionsStatus)

instance Prelude.FromJSON SnapshotOptionsStatus where
  parseJSON =
    Prelude.withObject
      "SnapshotOptionsStatus"
      ( \x ->
          SnapshotOptionsStatus'
            Prelude.<$> (x Prelude..: "Options")
            Prelude.<*> (x Prelude..: "Status")
      )

instance Prelude.Hashable SnapshotOptionsStatus

instance Prelude.NFData SnapshotOptionsStatus
