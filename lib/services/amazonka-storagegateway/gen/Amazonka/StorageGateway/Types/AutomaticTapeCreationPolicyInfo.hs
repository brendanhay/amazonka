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
-- Module      : Amazonka.StorageGateway.Types.AutomaticTapeCreationPolicyInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StorageGateway.Types.AutomaticTapeCreationPolicyInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.StorageGateway.Types.AutomaticTapeCreationRule

-- | Information about the gateway\'s automatic tape creation policies,
-- including the automatic tape creation rules and the gateway that is
-- using the policies.
--
-- /See:/ 'newAutomaticTapeCreationPolicyInfo' smart constructor.
data AutomaticTapeCreationPolicyInfo = AutomaticTapeCreationPolicyInfo'
  { gatewayARN :: Prelude.Maybe Prelude.Text,
    -- | An automatic tape creation policy consists of a list of automatic tape
    -- creation rules. This returns the rules that determine when and how to
    -- automatically create new tapes.
    automaticTapeCreationRules :: Prelude.Maybe (Prelude.NonEmpty AutomaticTapeCreationRule)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutomaticTapeCreationPolicyInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayARN', 'automaticTapeCreationPolicyInfo_gatewayARN' - Undocumented member.
--
-- 'automaticTapeCreationRules', 'automaticTapeCreationPolicyInfo_automaticTapeCreationRules' - An automatic tape creation policy consists of a list of automatic tape
-- creation rules. This returns the rules that determine when and how to
-- automatically create new tapes.
newAutomaticTapeCreationPolicyInfo ::
  AutomaticTapeCreationPolicyInfo
newAutomaticTapeCreationPolicyInfo =
  AutomaticTapeCreationPolicyInfo'
    { gatewayARN =
        Prelude.Nothing,
      automaticTapeCreationRules =
        Prelude.Nothing
    }

-- | Undocumented member.
automaticTapeCreationPolicyInfo_gatewayARN :: Lens.Lens' AutomaticTapeCreationPolicyInfo (Prelude.Maybe Prelude.Text)
automaticTapeCreationPolicyInfo_gatewayARN = Lens.lens (\AutomaticTapeCreationPolicyInfo' {gatewayARN} -> gatewayARN) (\s@AutomaticTapeCreationPolicyInfo' {} a -> s {gatewayARN = a} :: AutomaticTapeCreationPolicyInfo)

-- | An automatic tape creation policy consists of a list of automatic tape
-- creation rules. This returns the rules that determine when and how to
-- automatically create new tapes.
automaticTapeCreationPolicyInfo_automaticTapeCreationRules :: Lens.Lens' AutomaticTapeCreationPolicyInfo (Prelude.Maybe (Prelude.NonEmpty AutomaticTapeCreationRule))
automaticTapeCreationPolicyInfo_automaticTapeCreationRules = Lens.lens (\AutomaticTapeCreationPolicyInfo' {automaticTapeCreationRules} -> automaticTapeCreationRules) (\s@AutomaticTapeCreationPolicyInfo' {} a -> s {automaticTapeCreationRules = a} :: AutomaticTapeCreationPolicyInfo) Prelude.. Lens.mapping Lens.coerced

instance
  Core.FromJSON
    AutomaticTapeCreationPolicyInfo
  where
  parseJSON =
    Core.withObject
      "AutomaticTapeCreationPolicyInfo"
      ( \x ->
          AutomaticTapeCreationPolicyInfo'
            Prelude.<$> (x Core..:? "GatewayARN")
            Prelude.<*> (x Core..:? "AutomaticTapeCreationRules")
      )

instance
  Prelude.Hashable
    AutomaticTapeCreationPolicyInfo
  where
  hashWithSalt
    _salt
    AutomaticTapeCreationPolicyInfo' {..} =
      _salt `Prelude.hashWithSalt` gatewayARN
        `Prelude.hashWithSalt` automaticTapeCreationRules

instance
  Prelude.NFData
    AutomaticTapeCreationPolicyInfo
  where
  rnf AutomaticTapeCreationPolicyInfo' {..} =
    Prelude.rnf gatewayARN
      `Prelude.seq` Prelude.rnf automaticTapeCreationRules
