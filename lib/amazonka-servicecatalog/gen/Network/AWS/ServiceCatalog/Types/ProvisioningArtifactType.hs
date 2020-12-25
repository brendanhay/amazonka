{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisioningArtifactType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProvisioningArtifactType
  ( ProvisioningArtifactType
      ( ProvisioningArtifactType',
        ProvisioningArtifactTypeCloudFormationTemplate,
        ProvisioningArtifactTypeMarketplaceAmi,
        ProvisioningArtifactTypeMarketplaceCar,
        fromProvisioningArtifactType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ProvisioningArtifactType = ProvisioningArtifactType'
  { fromProvisioningArtifactType ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern ProvisioningArtifactTypeCloudFormationTemplate :: ProvisioningArtifactType
pattern ProvisioningArtifactTypeCloudFormationTemplate = ProvisioningArtifactType' "CLOUD_FORMATION_TEMPLATE"

pattern ProvisioningArtifactTypeMarketplaceAmi :: ProvisioningArtifactType
pattern ProvisioningArtifactTypeMarketplaceAmi = ProvisioningArtifactType' "MARKETPLACE_AMI"

pattern ProvisioningArtifactTypeMarketplaceCar :: ProvisioningArtifactType
pattern ProvisioningArtifactTypeMarketplaceCar = ProvisioningArtifactType' "MARKETPLACE_CAR"

{-# COMPLETE
  ProvisioningArtifactTypeCloudFormationTemplate,
  ProvisioningArtifactTypeMarketplaceAmi,
  ProvisioningArtifactTypeMarketplaceCar,
  ProvisioningArtifactType'
  #-}
