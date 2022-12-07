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
-- Module      : Amazonka.EC2.Types.IpamScope
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.IpamScope where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.IpamScopeState
import Amazonka.EC2.Types.IpamScopeType
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | In IPAM, a scope is the highest-level container within IPAM. An IPAM
-- contains two default scopes. Each scope represents the IP space for a
-- single network. The private scope is intended for all private IP address
-- space. The public scope is intended for all public IP address space.
-- Scopes enable you to reuse IP addresses across multiple unconnected
-- networks without causing IP address overlap or conflict.
--
-- For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/ipam/how-it-works-ipam.html How IPAM works>
-- in the /Amazon VPC IPAM User Guide/.
--
-- /See:/ 'newIpamScope' smart constructor.
data IpamScope = IpamScope'
  { -- | The key\/value combination of a tag assigned to the resource. Use the
    -- tag key in the filter name and the tag value as the filter value. For
    -- example, to find all resources that have a tag with the key @Owner@ and
    -- the value @TeamA@, specify @tag:Owner@ for the filter name and @TeamA@
    -- for the filter value.
    tags :: Prelude.Maybe [Tag],
    -- | The ARN of the scope.
    ipamScopeArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the scope.
    ipamScopeId :: Prelude.Maybe Prelude.Text,
    -- | The type of the scope.
    ipamScopeType :: Prelude.Maybe IpamScopeType,
    -- | The Amazon Web Services account ID of the owner of the scope.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The number of pools in the scope.
    poolCount :: Prelude.Maybe Prelude.Int,
    -- | The ARN of the IPAM.
    ipamArn :: Prelude.Maybe Prelude.Text,
    -- | The state of the IPAM scope.
    state :: Prelude.Maybe IpamScopeState,
    -- | The description of the scope.
    description :: Prelude.Maybe Prelude.Text,
    -- | Defines if the scope is the default scope or not.
    isDefault :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Web Services Region of the IPAM scope.
    ipamRegion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IpamScope' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'ipamScope_tags' - The key\/value combination of a tag assigned to the resource. Use the
-- tag key in the filter name and the tag value as the filter value. For
-- example, to find all resources that have a tag with the key @Owner@ and
-- the value @TeamA@, specify @tag:Owner@ for the filter name and @TeamA@
-- for the filter value.
--
-- 'ipamScopeArn', 'ipamScope_ipamScopeArn' - The ARN of the scope.
--
-- 'ipamScopeId', 'ipamScope_ipamScopeId' - The ID of the scope.
--
-- 'ipamScopeType', 'ipamScope_ipamScopeType' - The type of the scope.
--
-- 'ownerId', 'ipamScope_ownerId' - The Amazon Web Services account ID of the owner of the scope.
--
-- 'poolCount', 'ipamScope_poolCount' - The number of pools in the scope.
--
-- 'ipamArn', 'ipamScope_ipamArn' - The ARN of the IPAM.
--
-- 'state', 'ipamScope_state' - The state of the IPAM scope.
--
-- 'description', 'ipamScope_description' - The description of the scope.
--
-- 'isDefault', 'ipamScope_isDefault' - Defines if the scope is the default scope or not.
--
-- 'ipamRegion', 'ipamScope_ipamRegion' - The Amazon Web Services Region of the IPAM scope.
newIpamScope ::
  IpamScope
newIpamScope =
  IpamScope'
    { tags = Prelude.Nothing,
      ipamScopeArn = Prelude.Nothing,
      ipamScopeId = Prelude.Nothing,
      ipamScopeType = Prelude.Nothing,
      ownerId = Prelude.Nothing,
      poolCount = Prelude.Nothing,
      ipamArn = Prelude.Nothing,
      state = Prelude.Nothing,
      description = Prelude.Nothing,
      isDefault = Prelude.Nothing,
      ipamRegion = Prelude.Nothing
    }

-- | The key\/value combination of a tag assigned to the resource. Use the
-- tag key in the filter name and the tag value as the filter value. For
-- example, to find all resources that have a tag with the key @Owner@ and
-- the value @TeamA@, specify @tag:Owner@ for the filter name and @TeamA@
-- for the filter value.
ipamScope_tags :: Lens.Lens' IpamScope (Prelude.Maybe [Tag])
ipamScope_tags = Lens.lens (\IpamScope' {tags} -> tags) (\s@IpamScope' {} a -> s {tags = a} :: IpamScope) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the scope.
ipamScope_ipamScopeArn :: Lens.Lens' IpamScope (Prelude.Maybe Prelude.Text)
ipamScope_ipamScopeArn = Lens.lens (\IpamScope' {ipamScopeArn} -> ipamScopeArn) (\s@IpamScope' {} a -> s {ipamScopeArn = a} :: IpamScope)

-- | The ID of the scope.
ipamScope_ipamScopeId :: Lens.Lens' IpamScope (Prelude.Maybe Prelude.Text)
ipamScope_ipamScopeId = Lens.lens (\IpamScope' {ipamScopeId} -> ipamScopeId) (\s@IpamScope' {} a -> s {ipamScopeId = a} :: IpamScope)

-- | The type of the scope.
ipamScope_ipamScopeType :: Lens.Lens' IpamScope (Prelude.Maybe IpamScopeType)
ipamScope_ipamScopeType = Lens.lens (\IpamScope' {ipamScopeType} -> ipamScopeType) (\s@IpamScope' {} a -> s {ipamScopeType = a} :: IpamScope)

-- | The Amazon Web Services account ID of the owner of the scope.
ipamScope_ownerId :: Lens.Lens' IpamScope (Prelude.Maybe Prelude.Text)
ipamScope_ownerId = Lens.lens (\IpamScope' {ownerId} -> ownerId) (\s@IpamScope' {} a -> s {ownerId = a} :: IpamScope)

-- | The number of pools in the scope.
ipamScope_poolCount :: Lens.Lens' IpamScope (Prelude.Maybe Prelude.Int)
ipamScope_poolCount = Lens.lens (\IpamScope' {poolCount} -> poolCount) (\s@IpamScope' {} a -> s {poolCount = a} :: IpamScope)

-- | The ARN of the IPAM.
ipamScope_ipamArn :: Lens.Lens' IpamScope (Prelude.Maybe Prelude.Text)
ipamScope_ipamArn = Lens.lens (\IpamScope' {ipamArn} -> ipamArn) (\s@IpamScope' {} a -> s {ipamArn = a} :: IpamScope)

-- | The state of the IPAM scope.
ipamScope_state :: Lens.Lens' IpamScope (Prelude.Maybe IpamScopeState)
ipamScope_state = Lens.lens (\IpamScope' {state} -> state) (\s@IpamScope' {} a -> s {state = a} :: IpamScope)

-- | The description of the scope.
ipamScope_description :: Lens.Lens' IpamScope (Prelude.Maybe Prelude.Text)
ipamScope_description = Lens.lens (\IpamScope' {description} -> description) (\s@IpamScope' {} a -> s {description = a} :: IpamScope)

-- | Defines if the scope is the default scope or not.
ipamScope_isDefault :: Lens.Lens' IpamScope (Prelude.Maybe Prelude.Bool)
ipamScope_isDefault = Lens.lens (\IpamScope' {isDefault} -> isDefault) (\s@IpamScope' {} a -> s {isDefault = a} :: IpamScope)

-- | The Amazon Web Services Region of the IPAM scope.
ipamScope_ipamRegion :: Lens.Lens' IpamScope (Prelude.Maybe Prelude.Text)
ipamScope_ipamRegion = Lens.lens (\IpamScope' {ipamRegion} -> ipamRegion) (\s@IpamScope' {} a -> s {ipamRegion = a} :: IpamScope)

instance Data.FromXML IpamScope where
  parseXML x =
    IpamScope'
      Prelude.<$> ( x Data..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "ipamScopeArn")
      Prelude.<*> (x Data..@? "ipamScopeId")
      Prelude.<*> (x Data..@? "ipamScopeType")
      Prelude.<*> (x Data..@? "ownerId")
      Prelude.<*> (x Data..@? "poolCount")
      Prelude.<*> (x Data..@? "ipamArn")
      Prelude.<*> (x Data..@? "state")
      Prelude.<*> (x Data..@? "description")
      Prelude.<*> (x Data..@? "isDefault")
      Prelude.<*> (x Data..@? "ipamRegion")

instance Prelude.Hashable IpamScope where
  hashWithSalt _salt IpamScope' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` ipamScopeArn
      `Prelude.hashWithSalt` ipamScopeId
      `Prelude.hashWithSalt` ipamScopeType
      `Prelude.hashWithSalt` ownerId
      `Prelude.hashWithSalt` poolCount
      `Prelude.hashWithSalt` ipamArn
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` isDefault
      `Prelude.hashWithSalt` ipamRegion

instance Prelude.NFData IpamScope where
  rnf IpamScope' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf ipamScopeArn
      `Prelude.seq` Prelude.rnf ipamScopeId
      `Prelude.seq` Prelude.rnf ipamScopeType
      `Prelude.seq` Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf poolCount
      `Prelude.seq` Prelude.rnf ipamArn
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf isDefault
      `Prelude.seq` Prelude.rnf ipamRegion
