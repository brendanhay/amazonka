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
-- Module      : Network.AWS.AccessAnalyzer.Types.GeneratedPolicyProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AccessAnalyzer.Types.GeneratedPolicyProperties where

import Network.AWS.AccessAnalyzer.Types.CloudTrailProperties
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains the generated policy details.
--
-- /See:/ 'newGeneratedPolicyProperties' smart constructor.
data GeneratedPolicyProperties = GeneratedPolicyProperties'
  { -- | This value is set to @true@ if the generated policy contains all
    -- possible actions for a service that IAM Access Analyzer identified from
    -- the CloudTrail trail that you specified, and @false@ otherwise.
    isComplete :: Prelude.Maybe Prelude.Bool,
    -- | Lists details about the @Trail@ used to generated policy.
    cloudTrailProperties :: Prelude.Maybe CloudTrailProperties,
    -- | The ARN of the IAM entity (user or role) for which you are generating a
    -- policy.
    principalArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GeneratedPolicyProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isComplete', 'generatedPolicyProperties_isComplete' - This value is set to @true@ if the generated policy contains all
-- possible actions for a service that IAM Access Analyzer identified from
-- the CloudTrail trail that you specified, and @false@ otherwise.
--
-- 'cloudTrailProperties', 'generatedPolicyProperties_cloudTrailProperties' - Lists details about the @Trail@ used to generated policy.
--
-- 'principalArn', 'generatedPolicyProperties_principalArn' - The ARN of the IAM entity (user or role) for which you are generating a
-- policy.
newGeneratedPolicyProperties ::
  -- | 'principalArn'
  Prelude.Text ->
  GeneratedPolicyProperties
newGeneratedPolicyProperties pPrincipalArn_ =
  GeneratedPolicyProperties'
    { isComplete =
        Prelude.Nothing,
      cloudTrailProperties = Prelude.Nothing,
      principalArn = pPrincipalArn_
    }

-- | This value is set to @true@ if the generated policy contains all
-- possible actions for a service that IAM Access Analyzer identified from
-- the CloudTrail trail that you specified, and @false@ otherwise.
generatedPolicyProperties_isComplete :: Lens.Lens' GeneratedPolicyProperties (Prelude.Maybe Prelude.Bool)
generatedPolicyProperties_isComplete = Lens.lens (\GeneratedPolicyProperties' {isComplete} -> isComplete) (\s@GeneratedPolicyProperties' {} a -> s {isComplete = a} :: GeneratedPolicyProperties)

-- | Lists details about the @Trail@ used to generated policy.
generatedPolicyProperties_cloudTrailProperties :: Lens.Lens' GeneratedPolicyProperties (Prelude.Maybe CloudTrailProperties)
generatedPolicyProperties_cloudTrailProperties = Lens.lens (\GeneratedPolicyProperties' {cloudTrailProperties} -> cloudTrailProperties) (\s@GeneratedPolicyProperties' {} a -> s {cloudTrailProperties = a} :: GeneratedPolicyProperties)

-- | The ARN of the IAM entity (user or role) for which you are generating a
-- policy.
generatedPolicyProperties_principalArn :: Lens.Lens' GeneratedPolicyProperties Prelude.Text
generatedPolicyProperties_principalArn = Lens.lens (\GeneratedPolicyProperties' {principalArn} -> principalArn) (\s@GeneratedPolicyProperties' {} a -> s {principalArn = a} :: GeneratedPolicyProperties)

instance Core.FromJSON GeneratedPolicyProperties where
  parseJSON =
    Core.withObject
      "GeneratedPolicyProperties"
      ( \x ->
          GeneratedPolicyProperties'
            Prelude.<$> (x Core..:? "isComplete")
            Prelude.<*> (x Core..:? "cloudTrailProperties")
            Prelude.<*> (x Core..: "principalArn")
      )

instance Prelude.Hashable GeneratedPolicyProperties

instance Prelude.NFData GeneratedPolicyProperties
