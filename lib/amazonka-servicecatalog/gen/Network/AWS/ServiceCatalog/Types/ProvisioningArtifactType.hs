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
        PATCloudFormationTemplate,
        PATMarketplaceAMI,
        PATMarketplaceCar
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ProvisioningArtifactType = ProvisioningArtifactType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern PATCloudFormationTemplate :: ProvisioningArtifactType
pattern PATCloudFormationTemplate = ProvisioningArtifactType' "CLOUD_FORMATION_TEMPLATE"

pattern PATMarketplaceAMI :: ProvisioningArtifactType
pattern PATMarketplaceAMI = ProvisioningArtifactType' "MARKETPLACE_AMI"

pattern PATMarketplaceCar :: ProvisioningArtifactType
pattern PATMarketplaceCar = ProvisioningArtifactType' "MARKETPLACE_CAR"

{-# COMPLETE
  PATCloudFormationTemplate,
  PATMarketplaceAMI,
  PATMarketplaceCar,
  ProvisioningArtifactType'
  #-}
