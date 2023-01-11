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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
  { -- | The description of the security group rule.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the security group rule.
    securityGroupRuleId :: Prelude.Maybe Prelude.Text
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
-- 'description', 'securityGroupRuleDescription_description' - The description of the security group rule.
--
-- 'securityGroupRuleId', 'securityGroupRuleDescription_securityGroupRuleId' - The ID of the security group rule.
newSecurityGroupRuleDescription ::
  SecurityGroupRuleDescription
newSecurityGroupRuleDescription =
  SecurityGroupRuleDescription'
    { description =
        Prelude.Nothing,
      securityGroupRuleId = Prelude.Nothing
    }

-- | The description of the security group rule.
securityGroupRuleDescription_description :: Lens.Lens' SecurityGroupRuleDescription (Prelude.Maybe Prelude.Text)
securityGroupRuleDescription_description = Lens.lens (\SecurityGroupRuleDescription' {description} -> description) (\s@SecurityGroupRuleDescription' {} a -> s {description = a} :: SecurityGroupRuleDescription)

-- | The ID of the security group rule.
securityGroupRuleDescription_securityGroupRuleId :: Lens.Lens' SecurityGroupRuleDescription (Prelude.Maybe Prelude.Text)
securityGroupRuleDescription_securityGroupRuleId = Lens.lens (\SecurityGroupRuleDescription' {securityGroupRuleId} -> securityGroupRuleId) (\s@SecurityGroupRuleDescription' {} a -> s {securityGroupRuleId = a} :: SecurityGroupRuleDescription)

instance
  Prelude.Hashable
    SecurityGroupRuleDescription
  where
  hashWithSalt _salt SecurityGroupRuleDescription' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` securityGroupRuleId

instance Prelude.NFData SecurityGroupRuleDescription where
  rnf SecurityGroupRuleDescription' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf securityGroupRuleId

instance Data.ToQuery SecurityGroupRuleDescription where
  toQuery SecurityGroupRuleDescription' {..} =
    Prelude.mconcat
      [ "Description" Data.=: description,
        "SecurityGroupRuleId" Data.=: securityGroupRuleId
      ]
