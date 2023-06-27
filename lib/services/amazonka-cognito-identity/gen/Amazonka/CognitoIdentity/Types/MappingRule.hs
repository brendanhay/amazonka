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
-- Module      : Amazonka.CognitoIdentity.Types.MappingRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentity.Types.MappingRule where

import Amazonka.CognitoIdentity.Types.MappingRuleMatchType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A rule that maps a claim name, a claim value, and a match type to a role
-- ARN.
--
-- /See:/ 'newMappingRule' smart constructor.
data MappingRule = MappingRule'
  { -- | The claim name that must be present in the token, for example,
    -- \"isAdmin\" or \"paid\".
    claim :: Prelude.Text,
    -- | The match condition that specifies how closely the claim value in the
    -- IdP token must match @Value@.
    matchType :: MappingRuleMatchType,
    -- | A brief string that the claim must match, for example, \"paid\" or
    -- \"yes\".
    value :: Prelude.Text,
    -- | The role ARN.
    roleARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MappingRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'claim', 'mappingRule_claim' - The claim name that must be present in the token, for example,
-- \"isAdmin\" or \"paid\".
--
-- 'matchType', 'mappingRule_matchType' - The match condition that specifies how closely the claim value in the
-- IdP token must match @Value@.
--
-- 'value', 'mappingRule_value' - A brief string that the claim must match, for example, \"paid\" or
-- \"yes\".
--
-- 'roleARN', 'mappingRule_roleARN' - The role ARN.
newMappingRule ::
  -- | 'claim'
  Prelude.Text ->
  -- | 'matchType'
  MappingRuleMatchType ->
  -- | 'value'
  Prelude.Text ->
  -- | 'roleARN'
  Prelude.Text ->
  MappingRule
newMappingRule pClaim_ pMatchType_ pValue_ pRoleARN_ =
  MappingRule'
    { claim = pClaim_,
      matchType = pMatchType_,
      value = pValue_,
      roleARN = pRoleARN_
    }

-- | The claim name that must be present in the token, for example,
-- \"isAdmin\" or \"paid\".
mappingRule_claim :: Lens.Lens' MappingRule Prelude.Text
mappingRule_claim = Lens.lens (\MappingRule' {claim} -> claim) (\s@MappingRule' {} a -> s {claim = a} :: MappingRule)

-- | The match condition that specifies how closely the claim value in the
-- IdP token must match @Value@.
mappingRule_matchType :: Lens.Lens' MappingRule MappingRuleMatchType
mappingRule_matchType = Lens.lens (\MappingRule' {matchType} -> matchType) (\s@MappingRule' {} a -> s {matchType = a} :: MappingRule)

-- | A brief string that the claim must match, for example, \"paid\" or
-- \"yes\".
mappingRule_value :: Lens.Lens' MappingRule Prelude.Text
mappingRule_value = Lens.lens (\MappingRule' {value} -> value) (\s@MappingRule' {} a -> s {value = a} :: MappingRule)

-- | The role ARN.
mappingRule_roleARN :: Lens.Lens' MappingRule Prelude.Text
mappingRule_roleARN = Lens.lens (\MappingRule' {roleARN} -> roleARN) (\s@MappingRule' {} a -> s {roleARN = a} :: MappingRule)

instance Data.FromJSON MappingRule where
  parseJSON =
    Data.withObject
      "MappingRule"
      ( \x ->
          MappingRule'
            Prelude.<$> (x Data..: "Claim")
            Prelude.<*> (x Data..: "MatchType")
            Prelude.<*> (x Data..: "Value")
            Prelude.<*> (x Data..: "RoleARN")
      )

instance Prelude.Hashable MappingRule where
  hashWithSalt _salt MappingRule' {..} =
    _salt
      `Prelude.hashWithSalt` claim
      `Prelude.hashWithSalt` matchType
      `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` roleARN

instance Prelude.NFData MappingRule where
  rnf MappingRule' {..} =
    Prelude.rnf claim
      `Prelude.seq` Prelude.rnf matchType
      `Prelude.seq` Prelude.rnf value
      `Prelude.seq` Prelude.rnf roleARN

instance Data.ToJSON MappingRule where
  toJSON MappingRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Claim" Data..= claim),
            Prelude.Just ("MatchType" Data..= matchType),
            Prelude.Just ("Value" Data..= value),
            Prelude.Just ("RoleARN" Data..= roleARN)
          ]
      )
