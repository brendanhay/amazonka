{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.CodeSigningPolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.CodeSigningPolicies where

import Network.AWS.Lambda.Types.CodeSigningPolicy
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Code signing configuration policies specifies the validation failure action for signature mismatch or expiry.
--
--
--
-- /See:/ 'codeSigningPolicies' smart constructor.
newtype CodeSigningPolicies = CodeSigningPolicies'
  { _cspUntrustedArtifactOnDeployment ::
      Maybe CodeSigningPolicy
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CodeSigningPolicies' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cspUntrustedArtifactOnDeployment' - Code signing configuration policy for deployment validation failure. If you set the policy to @Enforce@ , Lambda blocks the deployment request if code-signing validation checks fail. If you set the policy to @Warn@ , Lambda allows the deployment and creates a CloudWatch log.  Default value: @Warn@
codeSigningPolicies ::
  CodeSigningPolicies
codeSigningPolicies =
  CodeSigningPolicies' {_cspUntrustedArtifactOnDeployment = Nothing}

-- | Code signing configuration policy for deployment validation failure. If you set the policy to @Enforce@ , Lambda blocks the deployment request if code-signing validation checks fail. If you set the policy to @Warn@ , Lambda allows the deployment and creates a CloudWatch log.  Default value: @Warn@
cspUntrustedArtifactOnDeployment :: Lens' CodeSigningPolicies (Maybe CodeSigningPolicy)
cspUntrustedArtifactOnDeployment = lens _cspUntrustedArtifactOnDeployment (\s a -> s {_cspUntrustedArtifactOnDeployment = a})

instance FromJSON CodeSigningPolicies where
  parseJSON =
    withObject
      "CodeSigningPolicies"
      ( \x ->
          CodeSigningPolicies' <$> (x .:? "UntrustedArtifactOnDeployment")
      )

instance Hashable CodeSigningPolicies

instance NFData CodeSigningPolicies

instance ToJSON CodeSigningPolicies where
  toJSON CodeSigningPolicies' {..} =
    object
      ( catMaybes
          [ ("UntrustedArtifactOnDeployment" .=)
              <$> _cspUntrustedArtifactOnDeployment
          ]
      )
