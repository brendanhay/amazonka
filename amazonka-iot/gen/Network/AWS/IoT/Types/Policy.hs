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
-- Module      : Network.AWS.IoT.Types.Policy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.Policy where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes an AWS IoT policy.
--
-- /See:/ 'newPolicy' smart constructor.
data Policy = Policy'
  { -- | The policy name.
    policyName :: Core.Maybe Core.Text,
    -- | The policy ARN.
    policyArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Policy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyName', 'policy_policyName' - The policy name.
--
-- 'policyArn', 'policy_policyArn' - The policy ARN.
newPolicy ::
  Policy
newPolicy =
  Policy'
    { policyName = Core.Nothing,
      policyArn = Core.Nothing
    }

-- | The policy name.
policy_policyName :: Lens.Lens' Policy (Core.Maybe Core.Text)
policy_policyName = Lens.lens (\Policy' {policyName} -> policyName) (\s@Policy' {} a -> s {policyName = a} :: Policy)

-- | The policy ARN.
policy_policyArn :: Lens.Lens' Policy (Core.Maybe Core.Text)
policy_policyArn = Lens.lens (\Policy' {policyArn} -> policyArn) (\s@Policy' {} a -> s {policyArn = a} :: Policy)

instance Core.FromJSON Policy where
  parseJSON =
    Core.withObject
      "Policy"
      ( \x ->
          Policy'
            Core.<$> (x Core..:? "policyName")
            Core.<*> (x Core..:? "policyArn")
      )

instance Core.Hashable Policy

instance Core.NFData Policy
