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
-- Module      : Amazonka.EC2.Types.SecurityGroupRuleDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.SecurityGroupRuleDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes the description of a security group rule.
--
-- You can use this when you want to update the security group rule
-- description for either an inbound or outbound rule.
--
-- /See:/ 'newSecurityGroupRuleDescription' smart constructor.
data SecurityGroupRuleDescription = SecurityGroupRuleDescription'
  { -- | The ID of the security group rule.
    securityGroupRuleId :: Prelude.Maybe Prelude.Text,
    -- | The description of the security group rule.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SecurityGroupRuleDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupRuleId', 'securityGroupRuleDescription_securityGroupRuleId' - The ID of the security group rule.
--
-- 'description', 'securityGroupRuleDescription_description' - The description of the security group rule.
newSecurityGroupRuleDescription ::
  SecurityGroupRuleDescription
newSecurityGroupRuleDescription =
  SecurityGroupRuleDescription'
    { securityGroupRuleId =
        Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The ID of the security group rule.
securityGroupRuleDescription_securityGroupRuleId :: Lens.Lens' SecurityGroupRuleDescription (Prelude.Maybe Prelude.Text)
securityGroupRuleDescription_securityGroupRuleId = Lens.lens (\SecurityGroupRuleDescription' {securityGroupRuleId} -> securityGroupRuleId) (\s@SecurityGroupRuleDescription' {} a -> s {securityGroupRuleId = a} :: SecurityGroupRuleDescription)

-- | The description of the security group rule.
securityGroupRuleDescription_description :: Lens.Lens' SecurityGroupRuleDescription (Prelude.Maybe Prelude.Text)
securityGroupRuleDescription_description = Lens.lens (\SecurityGroupRuleDescription' {description} -> description) (\s@SecurityGroupRuleDescription' {} a -> s {description = a} :: SecurityGroupRuleDescription)

instance
  Prelude.Hashable
    SecurityGroupRuleDescription
  where
  hashWithSalt _salt SecurityGroupRuleDescription' {..} =
    _salt `Prelude.hashWithSalt` securityGroupRuleId
      `Prelude.hashWithSalt` description

instance Prelude.NFData SecurityGroupRuleDescription where
  rnf SecurityGroupRuleDescription' {..} =
    Prelude.rnf securityGroupRuleId
      `Prelude.seq` Prelude.rnf description

instance Data.ToQuery SecurityGroupRuleDescription where
  toQuery SecurityGroupRuleDescription' {..} =
    Prelude.mconcat
      [ "SecurityGroupRuleId" Data.=: securityGroupRuleId,
        "Description" Data.=: description
      ]
