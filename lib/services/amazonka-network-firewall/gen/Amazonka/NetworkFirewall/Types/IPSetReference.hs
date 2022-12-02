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
-- Module      : Amazonka.NetworkFirewall.Types.IPSetReference
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.IPSetReference where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Configures one or more IP set references for a Suricata-compatible rule
-- group. This is used in CreateRuleGroup or UpdateRuleGroup. An IP set
-- reference is a rule variable that references a resource that you create
-- and manage in another Amazon Web Services service, such as an Amazon VPC
-- prefix list. Network Firewall IP set references enable you to
-- dynamically update the contents of your rules. When you create, update,
-- or delete the IP set you are referencing in your rule, Network Firewall
-- automatically updates the rule\'s content with the changes. For more
-- information about IP set references in Network Firewall, see
-- <https://docs.aws.amazon.com/network-firewall/latest/developerguide/rule-groups-ip-set-references Using IP set references>
-- in the /Network Firewall Developer Guide/.
--
-- Network Firewall currently supports only
-- <https://docs.aws.amazon.com/vpc/latest/userguide/managed-prefix-lists.html Amazon VPC prefix lists>
-- as IP set references.
--
-- /See:/ 'newIPSetReference' smart constructor.
data IPSetReference = IPSetReference'
  { -- | The Amazon Resource Name (ARN) of the resource that you are referencing
    -- in your rule group.
    referenceArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IPSetReference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'referenceArn', 'iPSetReference_referenceArn' - The Amazon Resource Name (ARN) of the resource that you are referencing
-- in your rule group.
newIPSetReference ::
  IPSetReference
newIPSetReference =
  IPSetReference' {referenceArn = Prelude.Nothing}

-- | The Amazon Resource Name (ARN) of the resource that you are referencing
-- in your rule group.
iPSetReference_referenceArn :: Lens.Lens' IPSetReference (Prelude.Maybe Prelude.Text)
iPSetReference_referenceArn = Lens.lens (\IPSetReference' {referenceArn} -> referenceArn) (\s@IPSetReference' {} a -> s {referenceArn = a} :: IPSetReference)

instance Data.FromJSON IPSetReference where
  parseJSON =
    Data.withObject
      "IPSetReference"
      ( \x ->
          IPSetReference'
            Prelude.<$> (x Data..:? "ReferenceArn")
      )

instance Prelude.Hashable IPSetReference where
  hashWithSalt _salt IPSetReference' {..} =
    _salt `Prelude.hashWithSalt` referenceArn

instance Prelude.NFData IPSetReference where
  rnf IPSetReference' {..} = Prelude.rnf referenceArn

instance Data.ToJSON IPSetReference where
  toJSON IPSetReference' {..} =
    Data.object
      ( Prelude.catMaybes
          [("ReferenceArn" Data..=) Prelude.<$> referenceArn]
      )
