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
-- Module      : Amazonka.AccessAnalyzer.Types.GeneratedPolicyResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.GeneratedPolicyResult where

import Amazonka.AccessAnalyzer.Types.GeneratedPolicy
import Amazonka.AccessAnalyzer.Types.GeneratedPolicyProperties
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the text for the generated policy and its details.
--
-- /See:/ 'newGeneratedPolicyResult' smart constructor.
data GeneratedPolicyResult = GeneratedPolicyResult'
  { -- | The text to use as the content for the new policy. The policy is created
    -- using the
    -- <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreatePolicy.html CreatePolicy>
    -- action.
    generatedPolicies :: Prelude.Maybe [GeneratedPolicy],
    -- | A @GeneratedPolicyProperties@ object that contains properties of the
    -- generated policy.
    properties :: GeneratedPolicyProperties
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GeneratedPolicyResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'generatedPolicies', 'generatedPolicyResult_generatedPolicies' - The text to use as the content for the new policy. The policy is created
-- using the
-- <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreatePolicy.html CreatePolicy>
-- action.
--
-- 'properties', 'generatedPolicyResult_properties' - A @GeneratedPolicyProperties@ object that contains properties of the
-- generated policy.
newGeneratedPolicyResult ::
  -- | 'properties'
  GeneratedPolicyProperties ->
  GeneratedPolicyResult
newGeneratedPolicyResult pProperties_ =
  GeneratedPolicyResult'
    { generatedPolicies =
        Prelude.Nothing,
      properties = pProperties_
    }

-- | The text to use as the content for the new policy. The policy is created
-- using the
-- <https://docs.aws.amazon.com/IAM/latest/APIReference/API_CreatePolicy.html CreatePolicy>
-- action.
generatedPolicyResult_generatedPolicies :: Lens.Lens' GeneratedPolicyResult (Prelude.Maybe [GeneratedPolicy])
generatedPolicyResult_generatedPolicies = Lens.lens (\GeneratedPolicyResult' {generatedPolicies} -> generatedPolicies) (\s@GeneratedPolicyResult' {} a -> s {generatedPolicies = a} :: GeneratedPolicyResult) Prelude.. Lens.mapping Lens.coerced

-- | A @GeneratedPolicyProperties@ object that contains properties of the
-- generated policy.
generatedPolicyResult_properties :: Lens.Lens' GeneratedPolicyResult GeneratedPolicyProperties
generatedPolicyResult_properties = Lens.lens (\GeneratedPolicyResult' {properties} -> properties) (\s@GeneratedPolicyResult' {} a -> s {properties = a} :: GeneratedPolicyResult)

instance Data.FromJSON GeneratedPolicyResult where
  parseJSON =
    Data.withObject
      "GeneratedPolicyResult"
      ( \x ->
          GeneratedPolicyResult'
            Prelude.<$> ( x
                            Data..:? "generatedPolicies"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "properties")
      )

instance Prelude.Hashable GeneratedPolicyResult where
  hashWithSalt _salt GeneratedPolicyResult' {..} =
    _salt
      `Prelude.hashWithSalt` generatedPolicies
      `Prelude.hashWithSalt` properties

instance Prelude.NFData GeneratedPolicyResult where
  rnf GeneratedPolicyResult' {..} =
    Prelude.rnf generatedPolicies `Prelude.seq`
      Prelude.rnf properties
