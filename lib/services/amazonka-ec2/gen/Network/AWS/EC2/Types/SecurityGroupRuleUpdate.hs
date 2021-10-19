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
-- Module      : Network.AWS.EC2.Types.SecurityGroupRuleUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SecurityGroupRuleUpdate where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.SecurityGroupRuleRequest
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an update to a security group rule.
--
-- /See:/ 'newSecurityGroupRuleUpdate' smart constructor.
data SecurityGroupRuleUpdate = SecurityGroupRuleUpdate'
  { -- | The ID of the security group rule.
    securityGroupRuleId :: Prelude.Maybe Prelude.Text,
    -- | Information about the security group rule.
    securityGroupRule :: Prelude.Maybe SecurityGroupRuleRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SecurityGroupRuleUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupRuleId', 'securityGroupRuleUpdate_securityGroupRuleId' - The ID of the security group rule.
--
-- 'securityGroupRule', 'securityGroupRuleUpdate_securityGroupRule' - Information about the security group rule.
newSecurityGroupRuleUpdate ::
  SecurityGroupRuleUpdate
newSecurityGroupRuleUpdate =
  SecurityGroupRuleUpdate'
    { securityGroupRuleId =
        Prelude.Nothing,
      securityGroupRule = Prelude.Nothing
    }

-- | The ID of the security group rule.
securityGroupRuleUpdate_securityGroupRuleId :: Lens.Lens' SecurityGroupRuleUpdate (Prelude.Maybe Prelude.Text)
securityGroupRuleUpdate_securityGroupRuleId = Lens.lens (\SecurityGroupRuleUpdate' {securityGroupRuleId} -> securityGroupRuleId) (\s@SecurityGroupRuleUpdate' {} a -> s {securityGroupRuleId = a} :: SecurityGroupRuleUpdate)

-- | Information about the security group rule.
securityGroupRuleUpdate_securityGroupRule :: Lens.Lens' SecurityGroupRuleUpdate (Prelude.Maybe SecurityGroupRuleRequest)
securityGroupRuleUpdate_securityGroupRule = Lens.lens (\SecurityGroupRuleUpdate' {securityGroupRule} -> securityGroupRule) (\s@SecurityGroupRuleUpdate' {} a -> s {securityGroupRule = a} :: SecurityGroupRuleUpdate)

instance Prelude.Hashable SecurityGroupRuleUpdate

instance Prelude.NFData SecurityGroupRuleUpdate

instance Core.ToQuery SecurityGroupRuleUpdate where
  toQuery SecurityGroupRuleUpdate' {..} =
    Prelude.mconcat
      [ "SecurityGroupRuleId" Core.=: securityGroupRuleId,
        "SecurityGroupRule" Core.=: securityGroupRule
      ]
