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
-- Module      : Amazonka.CloudSearch.Types.Limits
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudSearch.Types.Limits where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newLimits' smart constructor.
data Limits = Limits'
  { maximumReplicationCount :: Prelude.Natural,
    maximumPartitionCount :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Limits' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maximumReplicationCount', 'limits_maximumReplicationCount' - Undocumented member.
--
-- 'maximumPartitionCount', 'limits_maximumPartitionCount' - Undocumented member.
newLimits ::
  -- | 'maximumReplicationCount'
  Prelude.Natural ->
  -- | 'maximumPartitionCount'
  Prelude.Natural ->
  Limits
newLimits
  pMaximumReplicationCount_
  pMaximumPartitionCount_ =
    Limits'
      { maximumReplicationCount =
          pMaximumReplicationCount_,
        maximumPartitionCount = pMaximumPartitionCount_
      }

-- | Undocumented member.
limits_maximumReplicationCount :: Lens.Lens' Limits Prelude.Natural
limits_maximumReplicationCount = Lens.lens (\Limits' {maximumReplicationCount} -> maximumReplicationCount) (\s@Limits' {} a -> s {maximumReplicationCount = a} :: Limits)

-- | Undocumented member.
limits_maximumPartitionCount :: Lens.Lens' Limits Prelude.Natural
limits_maximumPartitionCount = Lens.lens (\Limits' {maximumPartitionCount} -> maximumPartitionCount) (\s@Limits' {} a -> s {maximumPartitionCount = a} :: Limits)

instance Data.FromXML Limits where
  parseXML x =
    Limits'
      Prelude.<$> (x Data..@ "MaximumReplicationCount")
      Prelude.<*> (x Data..@ "MaximumPartitionCount")

instance Prelude.Hashable Limits where
  hashWithSalt _salt Limits' {..} =
    _salt
      `Prelude.hashWithSalt` maximumReplicationCount
      `Prelude.hashWithSalt` maximumPartitionCount

instance Prelude.NFData Limits where
  rnf Limits' {..} =
    Prelude.rnf maximumReplicationCount
      `Prelude.seq` Prelude.rnf maximumPartitionCount
