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
-- Module      : Amazonka.Lambda.Types.CodeSigningPolicies
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types.CodeSigningPolicies where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Lambda.Types.CodeSigningPolicy
import qualified Amazonka.Prelude as Prelude

-- | Code signing configuration
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-codesigning.html#config-codesigning-policies policies>
-- specify the validation failure action for signature mismatch or expiry.
--
-- /See:/ 'newCodeSigningPolicies' smart constructor.
data CodeSigningPolicies = CodeSigningPolicies'
  { -- | Code signing configuration policy for deployment validation failure. If
    -- you set the policy to @Enforce@, Lambda blocks the deployment request if
    -- signature validation checks fail. If you set the policy to @Warn@,
    -- Lambda allows the deployment and creates a CloudWatch log.
    --
    -- Default value: @Warn@
    untrustedArtifactOnDeployment :: Prelude.Maybe CodeSigningPolicy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CodeSigningPolicies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'untrustedArtifactOnDeployment', 'codeSigningPolicies_untrustedArtifactOnDeployment' - Code signing configuration policy for deployment validation failure. If
-- you set the policy to @Enforce@, Lambda blocks the deployment request if
-- signature validation checks fail. If you set the policy to @Warn@,
-- Lambda allows the deployment and creates a CloudWatch log.
--
-- Default value: @Warn@
newCodeSigningPolicies ::
  CodeSigningPolicies
newCodeSigningPolicies =
  CodeSigningPolicies'
    { untrustedArtifactOnDeployment =
        Prelude.Nothing
    }

-- | Code signing configuration policy for deployment validation failure. If
-- you set the policy to @Enforce@, Lambda blocks the deployment request if
-- signature validation checks fail. If you set the policy to @Warn@,
-- Lambda allows the deployment and creates a CloudWatch log.
--
-- Default value: @Warn@
codeSigningPolicies_untrustedArtifactOnDeployment :: Lens.Lens' CodeSigningPolicies (Prelude.Maybe CodeSigningPolicy)
codeSigningPolicies_untrustedArtifactOnDeployment = Lens.lens (\CodeSigningPolicies' {untrustedArtifactOnDeployment} -> untrustedArtifactOnDeployment) (\s@CodeSigningPolicies' {} a -> s {untrustedArtifactOnDeployment = a} :: CodeSigningPolicies)

instance Core.FromJSON CodeSigningPolicies where
  parseJSON =
    Core.withObject
      "CodeSigningPolicies"
      ( \x ->
          CodeSigningPolicies'
            Prelude.<$> (x Core..:? "UntrustedArtifactOnDeployment")
      )

instance Prelude.Hashable CodeSigningPolicies where
  hashWithSalt _salt CodeSigningPolicies' {..} =
    _salt
      `Prelude.hashWithSalt` untrustedArtifactOnDeployment

instance Prelude.NFData CodeSigningPolicies where
  rnf CodeSigningPolicies' {..} =
    Prelude.rnf untrustedArtifactOnDeployment

instance Core.ToJSON CodeSigningPolicies where
  toJSON CodeSigningPolicies' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("UntrustedArtifactOnDeployment" Core..=)
              Prelude.<$> untrustedArtifactOnDeployment
          ]
      )
