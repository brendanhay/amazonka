{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisioningArtifactPreferences
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProvisioningArtifactPreferences where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The user-defined preferences that will be applied during product provisioning, unless overridden by @ProvisioningPreferences@ or @UpdateProvisioningPreferences@ .
--
--
-- For more information on maximum concurrent accounts and failure tolerance, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-concepts.html#stackset-ops-options Stack set operation options> in the /AWS CloudFormation User Guide/ .
--
--
-- /See:/ 'provisioningArtifactPreferences' smart constructor.
data ProvisioningArtifactPreferences = ProvisioningArtifactPreferences'
  { _papStackSetRegions ::
      !(Maybe [Text]),
    _papStackSetAccounts ::
      !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProvisioningArtifactPreferences' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'papStackSetRegions' - One or more AWS Regions where stack instances are deployed from the stack set. These regions can be scoped in @ProvisioningPreferences$StackSetRegions@ and @UpdateProvisioningPreferences$StackSetRegions@ . Applicable only to a @CFN_STACKSET@ provisioned product type.
--
-- * 'papStackSetAccounts' - One or more AWS accounts where stack instances are deployed from the stack set. These accounts can be scoped in @ProvisioningPreferences$StackSetAccounts@ and @UpdateProvisioningPreferences$StackSetAccounts@ . Applicable only to a @CFN_STACKSET@ provisioned product type.
provisioningArtifactPreferences ::
  ProvisioningArtifactPreferences
provisioningArtifactPreferences =
  ProvisioningArtifactPreferences'
    { _papStackSetRegions = Nothing,
      _papStackSetAccounts = Nothing
    }

-- | One or more AWS Regions where stack instances are deployed from the stack set. These regions can be scoped in @ProvisioningPreferences$StackSetRegions@ and @UpdateProvisioningPreferences$StackSetRegions@ . Applicable only to a @CFN_STACKSET@ provisioned product type.
papStackSetRegions :: Lens' ProvisioningArtifactPreferences [Text]
papStackSetRegions = lens _papStackSetRegions (\s a -> s {_papStackSetRegions = a}) . _Default . _Coerce

-- | One or more AWS accounts where stack instances are deployed from the stack set. These accounts can be scoped in @ProvisioningPreferences$StackSetAccounts@ and @UpdateProvisioningPreferences$StackSetAccounts@ . Applicable only to a @CFN_STACKSET@ provisioned product type.
papStackSetAccounts :: Lens' ProvisioningArtifactPreferences [Text]
papStackSetAccounts = lens _papStackSetAccounts (\s a -> s {_papStackSetAccounts = a}) . _Default . _Coerce

instance FromJSON ProvisioningArtifactPreferences where
  parseJSON =
    withObject
      "ProvisioningArtifactPreferences"
      ( \x ->
          ProvisioningArtifactPreferences'
            <$> (x .:? "StackSetRegions" .!= mempty)
            <*> (x .:? "StackSetAccounts" .!= mempty)
      )

instance Hashable ProvisioningArtifactPreferences

instance NFData ProvisioningArtifactPreferences
