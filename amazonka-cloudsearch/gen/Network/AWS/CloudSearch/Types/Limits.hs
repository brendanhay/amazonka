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
-- Module      : Network.AWS.CloudSearch.Types.Limits
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.Limits where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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

instance Core.FromXML Limits where
  parseXML x =
    Limits'
      Prelude.<$> (x Core..@ "MaximumReplicationCount")
      Prelude.<*> (x Core..@ "MaximumPartitionCount")

instance Prelude.Hashable Limits

instance Prelude.NFData Limits
