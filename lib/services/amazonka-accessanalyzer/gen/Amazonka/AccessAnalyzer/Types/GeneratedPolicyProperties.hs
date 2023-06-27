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
-- Module      : Amazonka.AccessAnalyzer.Types.GeneratedPolicyProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.GeneratedPolicyProperties where

import Amazonka.AccessAnalyzer.Types.CloudTrailProperties
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the generated policy details.
--
-- /See:/ 'newGeneratedPolicyProperties' smart constructor.
data GeneratedPolicyProperties = GeneratedPolicyProperties'
  { -- | Lists details about the @Trail@ used to generated policy.
    cloudTrailProperties :: Prelude.Maybe CloudTrailProperties,
    -- | This value is set to @true@ if the generated policy contains all
    -- possible actions for a service that IAM Access Analyzer identified from
    -- the CloudTrail trail that you specified, and @false@ otherwise.
    isComplete :: Prelude.Maybe Prelude.Bool,
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
-- 'cloudTrailProperties', 'generatedPolicyProperties_cloudTrailProperties' - Lists details about the @Trail@ used to generated policy.
--
-- 'isComplete', 'generatedPolicyProperties_isComplete' - This value is set to @true@ if the generated policy contains all
-- possible actions for a service that IAM Access Analyzer identified from
-- the CloudTrail trail that you specified, and @false@ otherwise.
--
-- 'principalArn', 'generatedPolicyProperties_principalArn' - The ARN of the IAM entity (user or role) for which you are generating a
-- policy.
newGeneratedPolicyProperties ::
  -- | 'principalArn'
  Prelude.Text ->
  GeneratedPolicyProperties
newGeneratedPolicyProperties pPrincipalArn_ =
  GeneratedPolicyProperties'
    { cloudTrailProperties =
        Prelude.Nothing,
      isComplete = Prelude.Nothing,
      principalArn = pPrincipalArn_
    }

-- | Lists details about the @Trail@ used to generated policy.
generatedPolicyProperties_cloudTrailProperties :: Lens.Lens' GeneratedPolicyProperties (Prelude.Maybe CloudTrailProperties)
generatedPolicyProperties_cloudTrailProperties = Lens.lens (\GeneratedPolicyProperties' {cloudTrailProperties} -> cloudTrailProperties) (\s@GeneratedPolicyProperties' {} a -> s {cloudTrailProperties = a} :: GeneratedPolicyProperties)

-- | This value is set to @true@ if the generated policy contains all
-- possible actions for a service that IAM Access Analyzer identified from
-- the CloudTrail trail that you specified, and @false@ otherwise.
generatedPolicyProperties_isComplete :: Lens.Lens' GeneratedPolicyProperties (Prelude.Maybe Prelude.Bool)
generatedPolicyProperties_isComplete = Lens.lens (\GeneratedPolicyProperties' {isComplete} -> isComplete) (\s@GeneratedPolicyProperties' {} a -> s {isComplete = a} :: GeneratedPolicyProperties)

-- | The ARN of the IAM entity (user or role) for which you are generating a
-- policy.
generatedPolicyProperties_principalArn :: Lens.Lens' GeneratedPolicyProperties Prelude.Text
generatedPolicyProperties_principalArn = Lens.lens (\GeneratedPolicyProperties' {principalArn} -> principalArn) (\s@GeneratedPolicyProperties' {} a -> s {principalArn = a} :: GeneratedPolicyProperties)

instance Data.FromJSON GeneratedPolicyProperties where
  parseJSON =
    Data.withObject
      "GeneratedPolicyProperties"
      ( \x ->
          GeneratedPolicyProperties'
            Prelude.<$> (x Data..:? "cloudTrailProperties")
            Prelude.<*> (x Data..:? "isComplete")
            Prelude.<*> (x Data..: "principalArn")
      )

instance Prelude.Hashable GeneratedPolicyProperties where
  hashWithSalt _salt GeneratedPolicyProperties' {..} =
    _salt
      `Prelude.hashWithSalt` cloudTrailProperties
      `Prelude.hashWithSalt` isComplete
      `Prelude.hashWithSalt` principalArn

instance Prelude.NFData GeneratedPolicyProperties where
  rnf GeneratedPolicyProperties' {..} =
    Prelude.rnf cloudTrailProperties
      `Prelude.seq` Prelude.rnf isComplete
      `Prelude.seq` Prelude.rnf principalArn
