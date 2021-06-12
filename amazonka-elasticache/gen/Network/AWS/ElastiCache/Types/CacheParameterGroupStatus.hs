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
-- Module      : Network.AWS.ElastiCache.Types.CacheParameterGroupStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.CacheParameterGroupStatus where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Status of the cache parameter group.
--
-- /See:/ 'newCacheParameterGroupStatus' smart constructor.
data CacheParameterGroupStatus = CacheParameterGroupStatus'
  { -- | The name of the cache parameter group.
    cacheParameterGroupName :: Core.Maybe Core.Text,
    -- | The status of parameter updates.
    parameterApplyStatus :: Core.Maybe Core.Text,
    -- | A list of the cache node IDs which need to be rebooted for parameter
    -- changes to be applied. A node ID is a numeric identifier (0001, 0002,
    -- etc.).
    cacheNodeIdsToReboot :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CacheParameterGroupStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cacheParameterGroupName', 'cacheParameterGroupStatus_cacheParameterGroupName' - The name of the cache parameter group.
--
-- 'parameterApplyStatus', 'cacheParameterGroupStatus_parameterApplyStatus' - The status of parameter updates.
--
-- 'cacheNodeIdsToReboot', 'cacheParameterGroupStatus_cacheNodeIdsToReboot' - A list of the cache node IDs which need to be rebooted for parameter
-- changes to be applied. A node ID is a numeric identifier (0001, 0002,
-- etc.).
newCacheParameterGroupStatus ::
  CacheParameterGroupStatus
newCacheParameterGroupStatus =
  CacheParameterGroupStatus'
    { cacheParameterGroupName =
        Core.Nothing,
      parameterApplyStatus = Core.Nothing,
      cacheNodeIdsToReboot = Core.Nothing
    }

-- | The name of the cache parameter group.
cacheParameterGroupStatus_cacheParameterGroupName :: Lens.Lens' CacheParameterGroupStatus (Core.Maybe Core.Text)
cacheParameterGroupStatus_cacheParameterGroupName = Lens.lens (\CacheParameterGroupStatus' {cacheParameterGroupName} -> cacheParameterGroupName) (\s@CacheParameterGroupStatus' {} a -> s {cacheParameterGroupName = a} :: CacheParameterGroupStatus)

-- | The status of parameter updates.
cacheParameterGroupStatus_parameterApplyStatus :: Lens.Lens' CacheParameterGroupStatus (Core.Maybe Core.Text)
cacheParameterGroupStatus_parameterApplyStatus = Lens.lens (\CacheParameterGroupStatus' {parameterApplyStatus} -> parameterApplyStatus) (\s@CacheParameterGroupStatus' {} a -> s {parameterApplyStatus = a} :: CacheParameterGroupStatus)

-- | A list of the cache node IDs which need to be rebooted for parameter
-- changes to be applied. A node ID is a numeric identifier (0001, 0002,
-- etc.).
cacheParameterGroupStatus_cacheNodeIdsToReboot :: Lens.Lens' CacheParameterGroupStatus (Core.Maybe [Core.Text])
cacheParameterGroupStatus_cacheNodeIdsToReboot = Lens.lens (\CacheParameterGroupStatus' {cacheNodeIdsToReboot} -> cacheNodeIdsToReboot) (\s@CacheParameterGroupStatus' {} a -> s {cacheNodeIdsToReboot = a} :: CacheParameterGroupStatus) Core.. Lens.mapping Lens._Coerce

instance Core.FromXML CacheParameterGroupStatus where
  parseXML x =
    CacheParameterGroupStatus'
      Core.<$> (x Core..@? "CacheParameterGroupName")
      Core.<*> (x Core..@? "ParameterApplyStatus")
      Core.<*> ( x Core..@? "CacheNodeIdsToReboot"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "CacheNodeId")
               )

instance Core.Hashable CacheParameterGroupStatus

instance Core.NFData CacheParameterGroupStatus
