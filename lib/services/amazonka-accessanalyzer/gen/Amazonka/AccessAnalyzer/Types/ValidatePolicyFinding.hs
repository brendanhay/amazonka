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
-- Module      : Amazonka.AccessAnalyzer.Types.ValidatePolicyFinding
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.ValidatePolicyFinding where

import Amazonka.AccessAnalyzer.Types.Location
import Amazonka.AccessAnalyzer.Types.ValidatePolicyFindingType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A finding in a policy. Each finding is an actionable recommendation that
-- can be used to improve the policy.
--
-- /See:/ 'newValidatePolicyFinding' smart constructor.
data ValidatePolicyFinding = ValidatePolicyFinding'
  { -- | A localized message that explains the finding and provides guidance on
    -- how to address it.
    findingDetails :: Prelude.Text,
    -- | The impact of the finding.
    --
    -- Security warnings report when the policy allows access that we consider
    -- overly permissive.
    --
    -- Errors report when a part of the policy is not functional.
    --
    -- Warnings report non-security issues when a policy does not conform to
    -- policy writing best practices.
    --
    -- Suggestions recommend stylistic improvements in the policy that do not
    -- impact access.
    findingType :: ValidatePolicyFindingType,
    -- | The issue code provides an identifier of the issue associated with this
    -- finding.
    issueCode :: Prelude.Text,
    -- | A link to additional documentation about the type of finding.
    learnMoreLink :: Prelude.Text,
    -- | The list of locations in the policy document that are related to the
    -- finding. The issue code provides a summary of an issue identified by the
    -- finding.
    locations :: [Location]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ValidatePolicyFinding' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'findingDetails', 'validatePolicyFinding_findingDetails' - A localized message that explains the finding and provides guidance on
-- how to address it.
--
-- 'findingType', 'validatePolicyFinding_findingType' - The impact of the finding.
--
-- Security warnings report when the policy allows access that we consider
-- overly permissive.
--
-- Errors report when a part of the policy is not functional.
--
-- Warnings report non-security issues when a policy does not conform to
-- policy writing best practices.
--
-- Suggestions recommend stylistic improvements in the policy that do not
-- impact access.
--
-- 'issueCode', 'validatePolicyFinding_issueCode' - The issue code provides an identifier of the issue associated with this
-- finding.
--
-- 'learnMoreLink', 'validatePolicyFinding_learnMoreLink' - A link to additional documentation about the type of finding.
--
-- 'locations', 'validatePolicyFinding_locations' - The list of locations in the policy document that are related to the
-- finding. The issue code provides a summary of an issue identified by the
-- finding.
newValidatePolicyFinding ::
  -- | 'findingDetails'
  Prelude.Text ->
  -- | 'findingType'
  ValidatePolicyFindingType ->
  -- | 'issueCode'
  Prelude.Text ->
  -- | 'learnMoreLink'
  Prelude.Text ->
  ValidatePolicyFinding
newValidatePolicyFinding
  pFindingDetails_
  pFindingType_
  pIssueCode_
  pLearnMoreLink_ =
    ValidatePolicyFinding'
      { findingDetails =
          pFindingDetails_,
        findingType = pFindingType_,
        issueCode = pIssueCode_,
        learnMoreLink = pLearnMoreLink_,
        locations = Prelude.mempty
      }

-- | A localized message that explains the finding and provides guidance on
-- how to address it.
validatePolicyFinding_findingDetails :: Lens.Lens' ValidatePolicyFinding Prelude.Text
validatePolicyFinding_findingDetails = Lens.lens (\ValidatePolicyFinding' {findingDetails} -> findingDetails) (\s@ValidatePolicyFinding' {} a -> s {findingDetails = a} :: ValidatePolicyFinding)

-- | The impact of the finding.
--
-- Security warnings report when the policy allows access that we consider
-- overly permissive.
--
-- Errors report when a part of the policy is not functional.
--
-- Warnings report non-security issues when a policy does not conform to
-- policy writing best practices.
--
-- Suggestions recommend stylistic improvements in the policy that do not
-- impact access.
validatePolicyFinding_findingType :: Lens.Lens' ValidatePolicyFinding ValidatePolicyFindingType
validatePolicyFinding_findingType = Lens.lens (\ValidatePolicyFinding' {findingType} -> findingType) (\s@ValidatePolicyFinding' {} a -> s {findingType = a} :: ValidatePolicyFinding)

-- | The issue code provides an identifier of the issue associated with this
-- finding.
validatePolicyFinding_issueCode :: Lens.Lens' ValidatePolicyFinding Prelude.Text
validatePolicyFinding_issueCode = Lens.lens (\ValidatePolicyFinding' {issueCode} -> issueCode) (\s@ValidatePolicyFinding' {} a -> s {issueCode = a} :: ValidatePolicyFinding)

-- | A link to additional documentation about the type of finding.
validatePolicyFinding_learnMoreLink :: Lens.Lens' ValidatePolicyFinding Prelude.Text
validatePolicyFinding_learnMoreLink = Lens.lens (\ValidatePolicyFinding' {learnMoreLink} -> learnMoreLink) (\s@ValidatePolicyFinding' {} a -> s {learnMoreLink = a} :: ValidatePolicyFinding)

-- | The list of locations in the policy document that are related to the
-- finding. The issue code provides a summary of an issue identified by the
-- finding.
validatePolicyFinding_locations :: Lens.Lens' ValidatePolicyFinding [Location]
validatePolicyFinding_locations = Lens.lens (\ValidatePolicyFinding' {locations} -> locations) (\s@ValidatePolicyFinding' {} a -> s {locations = a} :: ValidatePolicyFinding) Prelude.. Lens.coerced

instance Data.FromJSON ValidatePolicyFinding where
  parseJSON =
    Data.withObject
      "ValidatePolicyFinding"
      ( \x ->
          ValidatePolicyFinding'
            Prelude.<$> (x Data..: "findingDetails")
            Prelude.<*> (x Data..: "findingType")
            Prelude.<*> (x Data..: "issueCode")
            Prelude.<*> (x Data..: "learnMoreLink")
            Prelude.<*> (x Data..:? "locations" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ValidatePolicyFinding where
  hashWithSalt _salt ValidatePolicyFinding' {..} =
    _salt
      `Prelude.hashWithSalt` findingDetails
      `Prelude.hashWithSalt` findingType
      `Prelude.hashWithSalt` issueCode
      `Prelude.hashWithSalt` learnMoreLink
      `Prelude.hashWithSalt` locations

instance Prelude.NFData ValidatePolicyFinding where
  rnf ValidatePolicyFinding' {..} =
    Prelude.rnf findingDetails
      `Prelude.seq` Prelude.rnf findingType
      `Prelude.seq` Prelude.rnf issueCode
      `Prelude.seq` Prelude.rnf learnMoreLink
      `Prelude.seq` Prelude.rnf locations
