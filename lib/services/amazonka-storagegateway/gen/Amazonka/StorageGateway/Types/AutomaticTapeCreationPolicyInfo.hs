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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StorageGateway.Types.AutomaticTapeCreationPolicyInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.StorageGateway.Types.AutomaticTapeCreationRule

-- | Information about the gateway\'s automatic tape creation policies,
-- including the automatic tape creation rules and the gateway that is
-- using the policies.
--
-- /See:/ 'newAutomaticTapeCreationPolicyInfo' smart constructor.
data AutomaticTapeCreationPolicyInfo = AutomaticTapeCreationPolicyInfo'
  { -- | An automatic tape creation policy consists of a list of automatic tape
    -- creation rules. This returns the rules that determine when and how to
    -- automatically create new tapes.
    automaticTapeCreationRules :: Prelude.Maybe (Prelude.NonEmpty AutomaticTapeCreationRule),
    gatewayARN :: Prelude.Maybe Prelude.Text
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
-- 'automaticTapeCreationRules', 'automaticTapeCreationPolicyInfo_automaticTapeCreationRules' - An automatic tape creation policy consists of a list of automatic tape
-- creation rules. This returns the rules that determine when and how to
-- automatically create new tapes.
--
-- 'gatewayARN', 'automaticTapeCreationPolicyInfo_gatewayARN' - Undocumented member.
newAutomaticTapeCreationPolicyInfo ::
  AutomaticTapeCreationPolicyInfo
newAutomaticTapeCreationPolicyInfo =
  AutomaticTapeCreationPolicyInfo'
    { automaticTapeCreationRules =
        Prelude.Nothing,
      gatewayARN = Prelude.Nothing
    }

-- | An automatic tape creation policy consists of a list of automatic tape
-- creation rules. This returns the rules that determine when and how to
-- automatically create new tapes.
automaticTapeCreationPolicyInfo_automaticTapeCreationRules :: Lens.Lens' AutomaticTapeCreationPolicyInfo (Prelude.Maybe (Prelude.NonEmpty AutomaticTapeCreationRule))
automaticTapeCreationPolicyInfo_automaticTapeCreationRules = Lens.lens (\AutomaticTapeCreationPolicyInfo' {automaticTapeCreationRules} -> automaticTapeCreationRules) (\s@AutomaticTapeCreationPolicyInfo' {} a -> s {automaticTapeCreationRules = a} :: AutomaticTapeCreationPolicyInfo) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
automaticTapeCreationPolicyInfo_gatewayARN :: Lens.Lens' AutomaticTapeCreationPolicyInfo (Prelude.Maybe Prelude.Text)
automaticTapeCreationPolicyInfo_gatewayARN = Lens.lens (\AutomaticTapeCreationPolicyInfo' {gatewayARN} -> gatewayARN) (\s@AutomaticTapeCreationPolicyInfo' {} a -> s {gatewayARN = a} :: AutomaticTapeCreationPolicyInfo)

instance
  Data.FromJSON
    AutomaticTapeCreationPolicyInfo
  where
  parseJSON =
    Data.withObject
      "AutomaticTapeCreationPolicyInfo"
      ( \x ->
          AutomaticTapeCreationPolicyInfo'
            Prelude.<$> (x Data..:? "AutomaticTapeCreationRules")
            Prelude.<*> (x Data..:? "GatewayARN")
      )

instance
  Prelude.Hashable
    AutomaticTapeCreationPolicyInfo
  where
  hashWithSalt
    _salt
    AutomaticTapeCreationPolicyInfo' {..} =
      _salt
        `Prelude.hashWithSalt` automaticTapeCreationRules
        `Prelude.hashWithSalt` gatewayARN

instance
  Prelude.NFData
    AutomaticTapeCreationPolicyInfo
  where
  rnf AutomaticTapeCreationPolicyInfo' {..} =
    Prelude.rnf automaticTapeCreationRules `Prelude.seq`
      Prelude.rnf gatewayARN
