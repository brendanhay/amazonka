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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.CacheSecurityGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElastiCache.Types.EC2SecurityGroup
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
  { -- | The ARN of the cache security group,
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name of the cache security group.
    cacheSecurityGroupName :: Prelude.Maybe Prelude.Text,
    -- | The description of the cache security group.
    description :: Prelude.Maybe Prelude.Text,
    -- | A list of Amazon EC2 security groups that are associated with this cache
    -- security group.
    eC2SecurityGroups :: Prelude.Maybe [EC2SecurityGroup],
    -- | The Amazon account ID of the cache security group owner.
    ownerId :: Prelude.Maybe Prelude.Text
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
-- 'arn', 'cacheSecurityGroup_arn' - The ARN of the cache security group,
--
-- 'cacheSecurityGroupName', 'cacheSecurityGroup_cacheSecurityGroupName' - The name of the cache security group.
--
-- 'description', 'cacheSecurityGroup_description' - The description of the cache security group.
--
-- 'eC2SecurityGroups', 'cacheSecurityGroup_eC2SecurityGroups' - A list of Amazon EC2 security groups that are associated with this cache
-- security group.
--
-- 'ownerId', 'cacheSecurityGroup_ownerId' - The Amazon account ID of the cache security group owner.
newCacheSecurityGroup ::
  CacheSecurityGroup
newCacheSecurityGroup =
  CacheSecurityGroup'
    { arn = Prelude.Nothing,
      cacheSecurityGroupName = Prelude.Nothing,
      description = Prelude.Nothing,
      eC2SecurityGroups = Prelude.Nothing,
      ownerId = Prelude.Nothing
    }

-- | The ARN of the cache security group,
cacheSecurityGroup_arn :: Lens.Lens' CacheSecurityGroup (Prelude.Maybe Prelude.Text)
cacheSecurityGroup_arn = Lens.lens (\CacheSecurityGroup' {arn} -> arn) (\s@CacheSecurityGroup' {} a -> s {arn = a} :: CacheSecurityGroup)

-- | The name of the cache security group.
cacheSecurityGroup_cacheSecurityGroupName :: Lens.Lens' CacheSecurityGroup (Prelude.Maybe Prelude.Text)
cacheSecurityGroup_cacheSecurityGroupName = Lens.lens (\CacheSecurityGroup' {cacheSecurityGroupName} -> cacheSecurityGroupName) (\s@CacheSecurityGroup' {} a -> s {cacheSecurityGroupName = a} :: CacheSecurityGroup)

-- | The description of the cache security group.
cacheSecurityGroup_description :: Lens.Lens' CacheSecurityGroup (Prelude.Maybe Prelude.Text)
cacheSecurityGroup_description = Lens.lens (\CacheSecurityGroup' {description} -> description) (\s@CacheSecurityGroup' {} a -> s {description = a} :: CacheSecurityGroup)

-- | A list of Amazon EC2 security groups that are associated with this cache
-- security group.
cacheSecurityGroup_eC2SecurityGroups :: Lens.Lens' CacheSecurityGroup (Prelude.Maybe [EC2SecurityGroup])
cacheSecurityGroup_eC2SecurityGroups = Lens.lens (\CacheSecurityGroup' {eC2SecurityGroups} -> eC2SecurityGroups) (\s@CacheSecurityGroup' {} a -> s {eC2SecurityGroups = a} :: CacheSecurityGroup) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon account ID of the cache security group owner.
cacheSecurityGroup_ownerId :: Lens.Lens' CacheSecurityGroup (Prelude.Maybe Prelude.Text)
cacheSecurityGroup_ownerId = Lens.lens (\CacheSecurityGroup' {ownerId} -> ownerId) (\s@CacheSecurityGroup' {} a -> s {ownerId = a} :: CacheSecurityGroup)

instance Data.FromXML CacheSecurityGroup where
  parseXML x =
    CacheSecurityGroup'
      Prelude.<$> (x Data..@? "ARN")
      Prelude.<*> (x Data..@? "CacheSecurityGroupName")
      Prelude.<*> (x Data..@? "Description")
      Prelude.<*> ( x Data..@? "EC2SecurityGroups"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "EC2SecurityGroup")
                  )
      Prelude.<*> (x Data..@? "OwnerId")

instance Prelude.Hashable CacheSecurityGroup where
  hashWithSalt _salt CacheSecurityGroup' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` cacheSecurityGroupName
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` eC2SecurityGroups
      `Prelude.hashWithSalt` ownerId

instance Prelude.NFData CacheSecurityGroup where
  rnf CacheSecurityGroup' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf cacheSecurityGroupName
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf eC2SecurityGroups
      `Prelude.seq` Prelude.rnf ownerId
