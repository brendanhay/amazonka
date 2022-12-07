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
-- Module      : Amazonka.IoT.Types.Policy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.Policy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an IoT policy.
--
-- /See:/ 'newPolicy' smart constructor.
data Policy = Policy'
  { -- | The policy name.
    policyName :: Prelude.Maybe Prelude.Text,
    -- | The policy ARN.
    policyArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { policyName = Prelude.Nothing,
      policyArn = Prelude.Nothing
    }

-- | The policy name.
policy_policyName :: Lens.Lens' Policy (Prelude.Maybe Prelude.Text)
policy_policyName = Lens.lens (\Policy' {policyName} -> policyName) (\s@Policy' {} a -> s {policyName = a} :: Policy)

-- | The policy ARN.
policy_policyArn :: Lens.Lens' Policy (Prelude.Maybe Prelude.Text)
policy_policyArn = Lens.lens (\Policy' {policyArn} -> policyArn) (\s@Policy' {} a -> s {policyArn = a} :: Policy)

instance Data.FromJSON Policy where
  parseJSON =
    Data.withObject
      "Policy"
      ( \x ->
          Policy'
            Prelude.<$> (x Data..:? "policyName")
            Prelude.<*> (x Data..:? "policyArn")
      )

instance Prelude.Hashable Policy where
  hashWithSalt _salt Policy' {..} =
    _salt `Prelude.hashWithSalt` policyName
      `Prelude.hashWithSalt` policyArn

instance Prelude.NFData Policy where
  rnf Policy' {..} =
    Prelude.rnf policyName
      `Prelude.seq` Prelude.rnf policyArn
