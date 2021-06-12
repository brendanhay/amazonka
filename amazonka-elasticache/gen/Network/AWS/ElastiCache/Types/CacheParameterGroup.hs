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
-- Module      : Network.AWS.ElastiCache.Types.CacheParameterGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.CacheParameterGroup where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents the output of a @CreateCacheParameterGroup@ operation.
--
-- /See:/ 'newCacheParameterGroup' smart constructor.
data CacheParameterGroup = CacheParameterGroup'
  { -- | Indicates whether the parameter group is associated with a Global
    -- Datastore
    isGlobal :: Core.Maybe Core.Bool,
    -- | The ARN (Amazon Resource Name) of the cache parameter group.
    arn :: Core.Maybe Core.Text,
    -- | The name of the cache parameter group.
    cacheParameterGroupName :: Core.Maybe Core.Text,
    -- | The name of the cache parameter group family that this cache parameter
    -- group is compatible with.
    --
    -- Valid values are: @memcached1.4@ | @memcached1.5@ | @memcached1.6@ |
    -- @redis2.6@ | @redis2.8@ | @redis3.2@ | @redis4.0@ | @redis5.0@ |
    -- @redis6.x@ |
    cacheParameterGroupFamily :: Core.Maybe Core.Text,
    -- | The description for this cache parameter group.
    description :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CacheParameterGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isGlobal', 'cacheParameterGroup_isGlobal' - Indicates whether the parameter group is associated with a Global
-- Datastore
--
-- 'arn', 'cacheParameterGroup_arn' - The ARN (Amazon Resource Name) of the cache parameter group.
--
-- 'cacheParameterGroupName', 'cacheParameterGroup_cacheParameterGroupName' - The name of the cache parameter group.
--
-- 'cacheParameterGroupFamily', 'cacheParameterGroup_cacheParameterGroupFamily' - The name of the cache parameter group family that this cache parameter
-- group is compatible with.
--
-- Valid values are: @memcached1.4@ | @memcached1.5@ | @memcached1.6@ |
-- @redis2.6@ | @redis2.8@ | @redis3.2@ | @redis4.0@ | @redis5.0@ |
-- @redis6.x@ |
--
-- 'description', 'cacheParameterGroup_description' - The description for this cache parameter group.
newCacheParameterGroup ::
  CacheParameterGroup
newCacheParameterGroup =
  CacheParameterGroup'
    { isGlobal = Core.Nothing,
      arn = Core.Nothing,
      cacheParameterGroupName = Core.Nothing,
      cacheParameterGroupFamily = Core.Nothing,
      description = Core.Nothing
    }

-- | Indicates whether the parameter group is associated with a Global
-- Datastore
cacheParameterGroup_isGlobal :: Lens.Lens' CacheParameterGroup (Core.Maybe Core.Bool)
cacheParameterGroup_isGlobal = Lens.lens (\CacheParameterGroup' {isGlobal} -> isGlobal) (\s@CacheParameterGroup' {} a -> s {isGlobal = a} :: CacheParameterGroup)

-- | The ARN (Amazon Resource Name) of the cache parameter group.
cacheParameterGroup_arn :: Lens.Lens' CacheParameterGroup (Core.Maybe Core.Text)
cacheParameterGroup_arn = Lens.lens (\CacheParameterGroup' {arn} -> arn) (\s@CacheParameterGroup' {} a -> s {arn = a} :: CacheParameterGroup)

-- | The name of the cache parameter group.
cacheParameterGroup_cacheParameterGroupName :: Lens.Lens' CacheParameterGroup (Core.Maybe Core.Text)
cacheParameterGroup_cacheParameterGroupName = Lens.lens (\CacheParameterGroup' {cacheParameterGroupName} -> cacheParameterGroupName) (\s@CacheParameterGroup' {} a -> s {cacheParameterGroupName = a} :: CacheParameterGroup)

-- | The name of the cache parameter group family that this cache parameter
-- group is compatible with.
--
-- Valid values are: @memcached1.4@ | @memcached1.5@ | @memcached1.6@ |
-- @redis2.6@ | @redis2.8@ | @redis3.2@ | @redis4.0@ | @redis5.0@ |
-- @redis6.x@ |
cacheParameterGroup_cacheParameterGroupFamily :: Lens.Lens' CacheParameterGroup (Core.Maybe Core.Text)
cacheParameterGroup_cacheParameterGroupFamily = Lens.lens (\CacheParameterGroup' {cacheParameterGroupFamily} -> cacheParameterGroupFamily) (\s@CacheParameterGroup' {} a -> s {cacheParameterGroupFamily = a} :: CacheParameterGroup)

-- | The description for this cache parameter group.
cacheParameterGroup_description :: Lens.Lens' CacheParameterGroup (Core.Maybe Core.Text)
cacheParameterGroup_description = Lens.lens (\CacheParameterGroup' {description} -> description) (\s@CacheParameterGroup' {} a -> s {description = a} :: CacheParameterGroup)

instance Core.FromXML CacheParameterGroup where
  parseXML x =
    CacheParameterGroup'
      Core.<$> (x Core..@? "IsGlobal")
      Core.<*> (x Core..@? "ARN")
      Core.<*> (x Core..@? "CacheParameterGroupName")
      Core.<*> (x Core..@? "CacheParameterGroupFamily")
      Core.<*> (x Core..@? "Description")

instance Core.Hashable CacheParameterGroup

instance Core.NFData CacheParameterGroup
