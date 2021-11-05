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
-- Module      : Amazonka.ElastiCache.Types.CacheSecurityGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.CacheSecurityGroup where

import qualified Amazonka.Core as Core
import Amazonka.ElastiCache.Types.EC2SecurityGroup
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

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
  { -- | The name of the cache security group.
    cacheSecurityGroupName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the cache security group,
    arn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon account ID of the cache security group owner.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | A list of Amazon EC2 security groups that are associated with this cache
    -- security group.
    eC2SecurityGroups :: Prelude.Maybe [EC2SecurityGroup],
    -- | The description of the cache security group.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CacheSecurityGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cacheSecurityGroupName', 'cacheSecurityGroup_cacheSecurityGroupName' - The name of the cache security group.
--
-- 'arn', 'cacheSecurityGroup_arn' - The ARN of the cache security group,
--
-- 'ownerId', 'cacheSecurityGroup_ownerId' - The Amazon account ID of the cache security group owner.
--
-- 'eC2SecurityGroups', 'cacheSecurityGroup_eC2SecurityGroups' - A list of Amazon EC2 security groups that are associated with this cache
-- security group.
--
-- 'description', 'cacheSecurityGroup_description' - The description of the cache security group.
newCacheSecurityGroup ::
  CacheSecurityGroup
newCacheSecurityGroup =
  CacheSecurityGroup'
    { cacheSecurityGroupName =
        Prelude.Nothing,
      arn = Prelude.Nothing,
      ownerId = Prelude.Nothing,
      eC2SecurityGroups = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The name of the cache security group.
cacheSecurityGroup_cacheSecurityGroupName :: Lens.Lens' CacheSecurityGroup (Prelude.Maybe Prelude.Text)
cacheSecurityGroup_cacheSecurityGroupName = Lens.lens (\CacheSecurityGroup' {cacheSecurityGroupName} -> cacheSecurityGroupName) (\s@CacheSecurityGroup' {} a -> s {cacheSecurityGroupName = a} :: CacheSecurityGroup)

-- | The ARN of the cache security group,
cacheSecurityGroup_arn :: Lens.Lens' CacheSecurityGroup (Prelude.Maybe Prelude.Text)
cacheSecurityGroup_arn = Lens.lens (\CacheSecurityGroup' {arn} -> arn) (\s@CacheSecurityGroup' {} a -> s {arn = a} :: CacheSecurityGroup)

-- | The Amazon account ID of the cache security group owner.
cacheSecurityGroup_ownerId :: Lens.Lens' CacheSecurityGroup (Prelude.Maybe Prelude.Text)
cacheSecurityGroup_ownerId = Lens.lens (\CacheSecurityGroup' {ownerId} -> ownerId) (\s@CacheSecurityGroup' {} a -> s {ownerId = a} :: CacheSecurityGroup)

-- | A list of Amazon EC2 security groups that are associated with this cache
-- security group.
cacheSecurityGroup_eC2SecurityGroups :: Lens.Lens' CacheSecurityGroup (Prelude.Maybe [EC2SecurityGroup])
cacheSecurityGroup_eC2SecurityGroups = Lens.lens (\CacheSecurityGroup' {eC2SecurityGroups} -> eC2SecurityGroups) (\s@CacheSecurityGroup' {} a -> s {eC2SecurityGroups = a} :: CacheSecurityGroup) Prelude.. Lens.mapping Lens.coerced

-- | The description of the cache security group.
cacheSecurityGroup_description :: Lens.Lens' CacheSecurityGroup (Prelude.Maybe Prelude.Text)
cacheSecurityGroup_description = Lens.lens (\CacheSecurityGroup' {description} -> description) (\s@CacheSecurityGroup' {} a -> s {description = a} :: CacheSecurityGroup)

instance Core.FromXML CacheSecurityGroup where
  parseXML x =
    CacheSecurityGroup'
      Prelude.<$> (x Core..@? "CacheSecurityGroupName")
      Prelude.<*> (x Core..@? "ARN")
      Prelude.<*> (x Core..@? "OwnerId")
      Prelude.<*> ( x Core..@? "EC2SecurityGroups"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "EC2SecurityGroup")
                  )
      Prelude.<*> (x Core..@? "Description")

instance Prelude.Hashable CacheSecurityGroup

instance Prelude.NFData CacheSecurityGroup
