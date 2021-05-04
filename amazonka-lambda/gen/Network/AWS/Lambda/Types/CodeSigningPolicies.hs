{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Lambda.Types.CodeSigningPolicies
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.CodeSigningPolicies where

import Network.AWS.Lambda.Types.CodeSigningPolicy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Code signing configuration policies specifies the validation failure
-- action for signature mismatch or expiry.
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON CodeSigningPolicies where
  parseJSON =
    Prelude.withObject
      "CodeSigningPolicies"
      ( \x ->
          CodeSigningPolicies'
            Prelude.<$> (x Prelude..:? "UntrustedArtifactOnDeployment")
      )

instance Prelude.Hashable CodeSigningPolicies

instance Prelude.NFData CodeSigningPolicies

instance Prelude.ToJSON CodeSigningPolicies where
  toJSON CodeSigningPolicies' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("UntrustedArtifactOnDeployment" Prelude..=)
              Prelude.<$> untrustedArtifactOnDeployment
          ]
      )
