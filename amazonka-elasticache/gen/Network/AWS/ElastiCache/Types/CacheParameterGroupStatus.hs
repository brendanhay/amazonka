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
-- Module      : Network.AWS.ElastiCache.Types.CacheParameterGroupStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.CacheParameterGroupStatus where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Status of the cache parameter group.
--
-- /See:/ 'newCacheParameterGroupStatus' smart constructor.
data CacheParameterGroupStatus = CacheParameterGroupStatus'
  { -- | The name of the cache parameter group.
    cacheParameterGroupName :: Prelude.Maybe Prelude.Text,
    -- | The status of parameter updates.
    parameterApplyStatus :: Prelude.Maybe Prelude.Text,
    -- | A list of the cache node IDs which need to be rebooted for parameter
    -- changes to be applied. A node ID is a numeric identifier (0001, 0002,
    -- etc.).
    cacheNodeIdsToReboot :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      parameterApplyStatus = Prelude.Nothing,
      cacheNodeIdsToReboot = Prelude.Nothing
    }

-- | The name of the cache parameter group.
cacheParameterGroupStatus_cacheParameterGroupName :: Lens.Lens' CacheParameterGroupStatus (Prelude.Maybe Prelude.Text)
cacheParameterGroupStatus_cacheParameterGroupName = Lens.lens (\CacheParameterGroupStatus' {cacheParameterGroupName} -> cacheParameterGroupName) (\s@CacheParameterGroupStatus' {} a -> s {cacheParameterGroupName = a} :: CacheParameterGroupStatus)

-- | The status of parameter updates.
cacheParameterGroupStatus_parameterApplyStatus :: Lens.Lens' CacheParameterGroupStatus (Prelude.Maybe Prelude.Text)
cacheParameterGroupStatus_parameterApplyStatus = Lens.lens (\CacheParameterGroupStatus' {parameterApplyStatus} -> parameterApplyStatus) (\s@CacheParameterGroupStatus' {} a -> s {parameterApplyStatus = a} :: CacheParameterGroupStatus)

-- | A list of the cache node IDs which need to be rebooted for parameter
-- changes to be applied. A node ID is a numeric identifier (0001, 0002,
-- etc.).
cacheParameterGroupStatus_cacheNodeIdsToReboot :: Lens.Lens' CacheParameterGroupStatus (Prelude.Maybe [Prelude.Text])
cacheParameterGroupStatus_cacheNodeIdsToReboot = Lens.lens (\CacheParameterGroupStatus' {cacheNodeIdsToReboot} -> cacheNodeIdsToReboot) (\s@CacheParameterGroupStatus' {} a -> s {cacheNodeIdsToReboot = a} :: CacheParameterGroupStatus) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromXML CacheParameterGroupStatus where
  parseXML x =
    CacheParameterGroupStatus'
      Prelude.<$> (x Prelude..@? "CacheParameterGroupName")
      Prelude.<*> (x Prelude..@? "ParameterApplyStatus")
      Prelude.<*> ( x Prelude..@? "CacheNodeIdsToReboot"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "CacheNodeId")
                  )

instance Prelude.Hashable CacheParameterGroupStatus

instance Prelude.NFData CacheParameterGroupStatus
