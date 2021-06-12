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
-- Module      : Network.AWS.ElastiCache.Types.CacheParameterGroupNameMessage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.CacheParameterGroupNameMessage where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents the output of one of the following operations:
--
-- -   @ModifyCacheParameterGroup@
--
-- -   @ResetCacheParameterGroup@
--
-- /See:/ 'newCacheParameterGroupNameMessage' smart constructor.
data CacheParameterGroupNameMessage = CacheParameterGroupNameMessage'
  { -- | The name of the cache parameter group.
    cacheParameterGroupName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CacheParameterGroupNameMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cacheParameterGroupName', 'cacheParameterGroupNameMessage_cacheParameterGroupName' - The name of the cache parameter group.
newCacheParameterGroupNameMessage ::
  CacheParameterGroupNameMessage
newCacheParameterGroupNameMessage =
  CacheParameterGroupNameMessage'
    { cacheParameterGroupName =
        Core.Nothing
    }

-- | The name of the cache parameter group.
cacheParameterGroupNameMessage_cacheParameterGroupName :: Lens.Lens' CacheParameterGroupNameMessage (Core.Maybe Core.Text)
cacheParameterGroupNameMessage_cacheParameterGroupName = Lens.lens (\CacheParameterGroupNameMessage' {cacheParameterGroupName} -> cacheParameterGroupName) (\s@CacheParameterGroupNameMessage' {} a -> s {cacheParameterGroupName = a} :: CacheParameterGroupNameMessage)

instance Core.FromXML CacheParameterGroupNameMessage where
  parseXML x =
    CacheParameterGroupNameMessage'
      Core.<$> (x Core..@? "CacheParameterGroupName")

instance Core.Hashable CacheParameterGroupNameMessage

instance Core.NFData CacheParameterGroupNameMessage
