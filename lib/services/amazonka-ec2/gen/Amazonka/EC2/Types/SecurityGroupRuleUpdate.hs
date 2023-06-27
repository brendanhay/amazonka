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
-- Module      : Amazonka.EC2.Types.SecurityGroupRuleUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.SecurityGroupRuleUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.SecurityGroupRuleRequest
import qualified Amazonka.Prelude as Prelude

-- | Describes an update to a security group rule.
--
-- /See:/ 'newSecurityGroupRuleUpdate' smart constructor.
data SecurityGroupRuleUpdate = SecurityGroupRuleUpdate'
  { -- | Information about the security group rule.
    securityGroupRule :: Prelude.Maybe SecurityGroupRuleRequest,
    -- | The ID of the security group rule.
    securityGroupRuleId :: Prelude.Text
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
-- 'securityGroupRule', 'securityGroupRuleUpdate_securityGroupRule' - Information about the security group rule.
--
-- 'securityGroupRuleId', 'securityGroupRuleUpdate_securityGroupRuleId' - The ID of the security group rule.
newSecurityGroupRuleUpdate ::
  -- | 'securityGroupRuleId'
  Prelude.Text ->
  SecurityGroupRuleUpdate
newSecurityGroupRuleUpdate pSecurityGroupRuleId_ =
  SecurityGroupRuleUpdate'
    { securityGroupRule =
        Prelude.Nothing,
      securityGroupRuleId = pSecurityGroupRuleId_
    }

-- | Information about the security group rule.
securityGroupRuleUpdate_securityGroupRule :: Lens.Lens' SecurityGroupRuleUpdate (Prelude.Maybe SecurityGroupRuleRequest)
securityGroupRuleUpdate_securityGroupRule = Lens.lens (\SecurityGroupRuleUpdate' {securityGroupRule} -> securityGroupRule) (\s@SecurityGroupRuleUpdate' {} a -> s {securityGroupRule = a} :: SecurityGroupRuleUpdate)

-- | The ID of the security group rule.
securityGroupRuleUpdate_securityGroupRuleId :: Lens.Lens' SecurityGroupRuleUpdate Prelude.Text
securityGroupRuleUpdate_securityGroupRuleId = Lens.lens (\SecurityGroupRuleUpdate' {securityGroupRuleId} -> securityGroupRuleId) (\s@SecurityGroupRuleUpdate' {} a -> s {securityGroupRuleId = a} :: SecurityGroupRuleUpdate)

instance Prelude.Hashable SecurityGroupRuleUpdate where
  hashWithSalt _salt SecurityGroupRuleUpdate' {..} =
    _salt
      `Prelude.hashWithSalt` securityGroupRule
      `Prelude.hashWithSalt` securityGroupRuleId

instance Prelude.NFData SecurityGroupRuleUpdate where
  rnf SecurityGroupRuleUpdate' {..} =
    Prelude.rnf securityGroupRule
      `Prelude.seq` Prelude.rnf securityGroupRuleId

instance Data.ToQuery SecurityGroupRuleUpdate where
  toQuery SecurityGroupRuleUpdate' {..} =
    Prelude.mconcat
      [ "SecurityGroupRule" Data.=: securityGroupRule,
        "SecurityGroupRuleId" Data.=: securityGroupRuleId
      ]
