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
-- Module      : Network.AWS.ElastiCache.Types.CacheSecurityGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.CacheSecurityGroup where

import qualified Network.AWS.Core as Core
import Network.AWS.ElastiCache.Types.EC2SecurityGroup
import qualified Network.AWS.Lens as Lens

-- | Represents the output of one of the following operations:
--
-- -   @AuthorizeCacheSecurityGroupIngress@
--
-- -   @CreateCacheSecurityGroup@
--
-- -   @RevokeCacheSecurityGroupIngress@
--
-- /See:/ 'newCacheSecurityGroup' smart constructor.
data CacheSecurityGroup = CacheSecurityGroup'
  { -- | The AWS account ID of the cache security group owner.
    ownerId :: Core.Maybe Core.Text,
    -- | The ARN of the cache security group,
    arn :: Core.Maybe Core.Text,
    -- | The name of the cache security group.
    cacheSecurityGroupName :: Core.Maybe Core.Text,
    -- | A list of Amazon EC2 security groups that are associated with this cache
    -- security group.
    eC2SecurityGroups :: Core.Maybe [EC2SecurityGroup],
    -- | The description of the cache security group.
    description :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CacheSecurityGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ownerId', 'cacheSecurityGroup_ownerId' - The AWS account ID of the cache security group owner.
--
-- 'arn', 'cacheSecurityGroup_arn' - The ARN of the cache security group,
--
-- 'cacheSecurityGroupName', 'cacheSecurityGroup_cacheSecurityGroupName' - The name of the cache security group.
--
-- 'eC2SecurityGroups', 'cacheSecurityGroup_eC2SecurityGroups' - A list of Amazon EC2 security groups that are associated with this cache
-- security group.
--
-- 'description', 'cacheSecurityGroup_description' - The description of the cache security group.
newCacheSecurityGroup ::
  CacheSecurityGroup
newCacheSecurityGroup =
  CacheSecurityGroup'
    { ownerId = Core.Nothing,
      arn = Core.Nothing,
      cacheSecurityGroupName = Core.Nothing,
      eC2SecurityGroups = Core.Nothing,
      description = Core.Nothing
    }

-- | The AWS account ID of the cache security group owner.
cacheSecurityGroup_ownerId :: Lens.Lens' CacheSecurityGroup (Core.Maybe Core.Text)
cacheSecurityGroup_ownerId = Lens.lens (\CacheSecurityGroup' {ownerId} -> ownerId) (\s@CacheSecurityGroup' {} a -> s {ownerId = a} :: CacheSecurityGroup)

-- | The ARN of the cache security group,
cacheSecurityGroup_arn :: Lens.Lens' CacheSecurityGroup (Core.Maybe Core.Text)
cacheSecurityGroup_arn = Lens.lens (\CacheSecurityGroup' {arn} -> arn) (\s@CacheSecurityGroup' {} a -> s {arn = a} :: CacheSecurityGroup)

-- | The name of the cache security group.
cacheSecurityGroup_cacheSecurityGroupName :: Lens.Lens' CacheSecurityGroup (Core.Maybe Core.Text)
cacheSecurityGroup_cacheSecurityGroupName = Lens.lens (\CacheSecurityGroup' {cacheSecurityGroupName} -> cacheSecurityGroupName) (\s@CacheSecurityGroup' {} a -> s {cacheSecurityGroupName = a} :: CacheSecurityGroup)

-- | A list of Amazon EC2 security groups that are associated with this cache
-- security group.
cacheSecurityGroup_eC2SecurityGroups :: Lens.Lens' CacheSecurityGroup (Core.Maybe [EC2SecurityGroup])
cacheSecurityGroup_eC2SecurityGroups = Lens.lens (\CacheSecurityGroup' {eC2SecurityGroups} -> eC2SecurityGroups) (\s@CacheSecurityGroup' {} a -> s {eC2SecurityGroups = a} :: CacheSecurityGroup) Core.. Lens.mapping Lens._Coerce

-- | The description of the cache security group.
cacheSecurityGroup_description :: Lens.Lens' CacheSecurityGroup (Core.Maybe Core.Text)
cacheSecurityGroup_description = Lens.lens (\CacheSecurityGroup' {description} -> description) (\s@CacheSecurityGroup' {} a -> s {description = a} :: CacheSecurityGroup)

instance Core.FromXML CacheSecurityGroup where
  parseXML x =
    CacheSecurityGroup'
      Core.<$> (x Core..@? "OwnerId")
      Core.<*> (x Core..@? "ARN")
      Core.<*> (x Core..@? "CacheSecurityGroupName")
      Core.<*> ( x Core..@? "EC2SecurityGroups" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "EC2SecurityGroup")
               )
      Core.<*> (x Core..@? "Description")

instance Core.Hashable CacheSecurityGroup

instance Core.NFData CacheSecurityGroup
