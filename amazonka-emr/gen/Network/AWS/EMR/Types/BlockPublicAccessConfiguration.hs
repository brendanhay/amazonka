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
-- Module      : Network.AWS.EMR.Types.BlockPublicAccessConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.BlockPublicAccessConfiguration where

import Network.AWS.EMR.Types.PortRange
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A configuration for Amazon EMR block public access. When
-- @BlockPublicSecurityGroupRules@ is set to @true@, Amazon EMR prevents
-- cluster creation if one of the cluster\'s security groups has a rule
-- that allows inbound traffic from 0.0.0.0\/0 or ::\/0 on a port, unless
-- the port is specified as an exception using
-- @PermittedPublicSecurityGroupRuleRanges@.
--
-- /See:/ 'newBlockPublicAccessConfiguration' smart constructor.
data BlockPublicAccessConfiguration = BlockPublicAccessConfiguration'
  { -- | Specifies ports and port ranges that are permitted to have security
    -- group rules that allow inbound traffic from all public sources. For
    -- example, if Port 23 (Telnet) is specified for
    -- @PermittedPublicSecurityGroupRuleRanges@, Amazon EMR allows cluster
    -- creation if a security group associated with the cluster has a rule that
    -- allows inbound traffic on Port 23 from IPv4 0.0.0.0\/0 or IPv6 port
    -- ::\/0 as the source.
    --
    -- By default, Port 22, which is used for SSH access to the cluster EC2
    -- instances, is in the list of @PermittedPublicSecurityGroupRuleRanges@.
    permittedPublicSecurityGroupRuleRanges :: Prelude.Maybe [PortRange],
    -- | Indicates whether Amazon EMR block public access is enabled (@true@) or
    -- disabled (@false@). By default, the value is @false@ for accounts that
    -- have created EMR clusters before July 2019. For accounts created after
    -- this, the default is @true@.
    blockPublicSecurityGroupRules :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BlockPublicAccessConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'permittedPublicSecurityGroupRuleRanges', 'blockPublicAccessConfiguration_permittedPublicSecurityGroupRuleRanges' - Specifies ports and port ranges that are permitted to have security
-- group rules that allow inbound traffic from all public sources. For
-- example, if Port 23 (Telnet) is specified for
-- @PermittedPublicSecurityGroupRuleRanges@, Amazon EMR allows cluster
-- creation if a security group associated with the cluster has a rule that
-- allows inbound traffic on Port 23 from IPv4 0.0.0.0\/0 or IPv6 port
-- ::\/0 as the source.
--
-- By default, Port 22, which is used for SSH access to the cluster EC2
-- instances, is in the list of @PermittedPublicSecurityGroupRuleRanges@.
--
-- 'blockPublicSecurityGroupRules', 'blockPublicAccessConfiguration_blockPublicSecurityGroupRules' - Indicates whether Amazon EMR block public access is enabled (@true@) or
-- disabled (@false@). By default, the value is @false@ for accounts that
-- have created EMR clusters before July 2019. For accounts created after
-- this, the default is @true@.
newBlockPublicAccessConfiguration ::
  -- | 'blockPublicSecurityGroupRules'
  Prelude.Bool ->
  BlockPublicAccessConfiguration
newBlockPublicAccessConfiguration
  pBlockPublicSecurityGroupRules_ =
    BlockPublicAccessConfiguration'
      { permittedPublicSecurityGroupRuleRanges =
          Prelude.Nothing,
        blockPublicSecurityGroupRules =
          pBlockPublicSecurityGroupRules_
      }

-- | Specifies ports and port ranges that are permitted to have security
-- group rules that allow inbound traffic from all public sources. For
-- example, if Port 23 (Telnet) is specified for
-- @PermittedPublicSecurityGroupRuleRanges@, Amazon EMR allows cluster
-- creation if a security group associated with the cluster has a rule that
-- allows inbound traffic on Port 23 from IPv4 0.0.0.0\/0 or IPv6 port
-- ::\/0 as the source.
--
-- By default, Port 22, which is used for SSH access to the cluster EC2
-- instances, is in the list of @PermittedPublicSecurityGroupRuleRanges@.
blockPublicAccessConfiguration_permittedPublicSecurityGroupRuleRanges :: Lens.Lens' BlockPublicAccessConfiguration (Prelude.Maybe [PortRange])
blockPublicAccessConfiguration_permittedPublicSecurityGroupRuleRanges = Lens.lens (\BlockPublicAccessConfiguration' {permittedPublicSecurityGroupRuleRanges} -> permittedPublicSecurityGroupRuleRanges) (\s@BlockPublicAccessConfiguration' {} a -> s {permittedPublicSecurityGroupRuleRanges = a} :: BlockPublicAccessConfiguration) Prelude.. Lens.mapping Prelude._Coerce

-- | Indicates whether Amazon EMR block public access is enabled (@true@) or
-- disabled (@false@). By default, the value is @false@ for accounts that
-- have created EMR clusters before July 2019. For accounts created after
-- this, the default is @true@.
blockPublicAccessConfiguration_blockPublicSecurityGroupRules :: Lens.Lens' BlockPublicAccessConfiguration Prelude.Bool
blockPublicAccessConfiguration_blockPublicSecurityGroupRules = Lens.lens (\BlockPublicAccessConfiguration' {blockPublicSecurityGroupRules} -> blockPublicSecurityGroupRules) (\s@BlockPublicAccessConfiguration' {} a -> s {blockPublicSecurityGroupRules = a} :: BlockPublicAccessConfiguration)

instance
  Prelude.FromJSON
    BlockPublicAccessConfiguration
  where
  parseJSON =
    Prelude.withObject
      "BlockPublicAccessConfiguration"
      ( \x ->
          BlockPublicAccessConfiguration'
            Prelude.<$> ( x
                            Prelude..:? "PermittedPublicSecurityGroupRuleRanges"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..: "BlockPublicSecurityGroupRules")
      )

instance
  Prelude.Hashable
    BlockPublicAccessConfiguration

instance
  Prelude.NFData
    BlockPublicAccessConfiguration

instance
  Prelude.ToJSON
    BlockPublicAccessConfiguration
  where
  toJSON BlockPublicAccessConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ( "PermittedPublicSecurityGroupRuleRanges"
                Prelude..=
            )
              Prelude.<$> permittedPublicSecurityGroupRuleRanges,
            Prelude.Just
              ( "BlockPublicSecurityGroupRules"
                  Prelude..= blockPublicSecurityGroupRules
              )
          ]
      )
