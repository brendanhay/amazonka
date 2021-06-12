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

-- | /See:/ 'newLimits' smart constructor.
data Limits = Limits'
  { maximumReplicationCount :: Core.Natural,
    maximumPartitionCount :: Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Natural ->
  -- | 'maximumPartitionCount'
  Core.Natural ->
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
limits_maximumReplicationCount :: Lens.Lens' Limits Core.Natural
limits_maximumReplicationCount = Lens.lens (\Limits' {maximumReplicationCount} -> maximumReplicationCount) (\s@Limits' {} a -> s {maximumReplicationCount = a} :: Limits)

-- | Undocumented member.
limits_maximumPartitionCount :: Lens.Lens' Limits Core.Natural
limits_maximumPartitionCount = Lens.lens (\Limits' {maximumPartitionCount} -> maximumPartitionCount) (\s@Limits' {} a -> s {maximumPartitionCount = a} :: Limits)

instance Core.FromXML Limits where
  parseXML x =
    Limits'
      Core.<$> (x Core..@ "MaximumReplicationCount")
      Core.<*> (x Core..@ "MaximumPartitionCount")

instance Core.Hashable Limits

instance Core.NFData Limits
