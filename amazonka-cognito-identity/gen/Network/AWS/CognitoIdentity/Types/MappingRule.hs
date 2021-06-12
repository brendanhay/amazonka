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
-- Module      : Network.AWS.CognitoIdentity.Types.MappingRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentity.Types.MappingRule where

import Network.AWS.CognitoIdentity.Types.MappingRuleMatchType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A rule that maps a claim name, a claim value, and a match type to a role
-- ARN.
--
-- /See:/ 'newMappingRule' smart constructor.
data MappingRule = MappingRule'
  { -- | The claim name that must be present in the token, for example,
    -- \"isAdmin\" or \"paid\".
    claim :: Core.Text,
    -- | The match condition that specifies how closely the claim value in the
    -- IdP token must match @Value@.
    matchType :: MappingRuleMatchType,
    -- | A brief string that the claim must match, for example, \"paid\" or
    -- \"yes\".
    value :: Core.Text,
    -- | The role ARN.
    roleARN :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'matchType'
  MappingRuleMatchType ->
  -- | 'value'
  Core.Text ->
  -- | 'roleARN'
  Core.Text ->
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
mappingRule_claim :: Lens.Lens' MappingRule Core.Text
mappingRule_claim = Lens.lens (\MappingRule' {claim} -> claim) (\s@MappingRule' {} a -> s {claim = a} :: MappingRule)

-- | The match condition that specifies how closely the claim value in the
-- IdP token must match @Value@.
mappingRule_matchType :: Lens.Lens' MappingRule MappingRuleMatchType
mappingRule_matchType = Lens.lens (\MappingRule' {matchType} -> matchType) (\s@MappingRule' {} a -> s {matchType = a} :: MappingRule)

-- | A brief string that the claim must match, for example, \"paid\" or
-- \"yes\".
mappingRule_value :: Lens.Lens' MappingRule Core.Text
mappingRule_value = Lens.lens (\MappingRule' {value} -> value) (\s@MappingRule' {} a -> s {value = a} :: MappingRule)

-- | The role ARN.
mappingRule_roleARN :: Lens.Lens' MappingRule Core.Text
mappingRule_roleARN = Lens.lens (\MappingRule' {roleARN} -> roleARN) (\s@MappingRule' {} a -> s {roleARN = a} :: MappingRule)

instance Core.FromJSON MappingRule where
  parseJSON =
    Core.withObject
      "MappingRule"
      ( \x ->
          MappingRule'
            Core.<$> (x Core..: "Claim")
            Core.<*> (x Core..: "MatchType")
            Core.<*> (x Core..: "Value")
            Core.<*> (x Core..: "RoleARN")
      )

instance Core.Hashable MappingRule

instance Core.NFData MappingRule

instance Core.ToJSON MappingRule where
  toJSON MappingRule' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Claim" Core..= claim),
            Core.Just ("MatchType" Core..= matchType),
            Core.Just ("Value" Core..= value),
            Core.Just ("RoleARN" Core..= roleARN)
          ]
      )
