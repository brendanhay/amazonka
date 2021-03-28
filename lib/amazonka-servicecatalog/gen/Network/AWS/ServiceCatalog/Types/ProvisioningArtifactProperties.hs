{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisioningArtifactProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServiceCatalog.Types.ProvisioningArtifactProperties
  ( ProvisioningArtifactProperties (..)
  -- * Smart constructor
  , mkProvisioningArtifactProperties
  -- * Lenses
  , pInfo
  , pDescription
  , pDisableTemplateValidation
  , pName
  , pType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ServiceCatalog.Types.ProvisioningArtifactDescription as Types
import qualified Network.AWS.ServiceCatalog.Types.ProvisioningArtifactInfoKey as Types
import qualified Network.AWS.ServiceCatalog.Types.ProvisioningArtifactInfoValue as Types
import qualified Network.AWS.ServiceCatalog.Types.ProvisioningArtifactName as Types
import qualified Network.AWS.ServiceCatalog.Types.ProvisioningArtifactType as Types

-- | Information about a provisioning artifact (also known as a version) for a product.
--
-- /See:/ 'mkProvisioningArtifactProperties' smart constructor.
data ProvisioningArtifactProperties = ProvisioningArtifactProperties'
  { info :: Core.HashMap Types.ProvisioningArtifactInfoKey Types.ProvisioningArtifactInfoValue
    -- ^ The URL of the CloudFormation template in Amazon S3. Specify the URL in JSON format as follows:
--
-- @"LoadTemplateFromURL": "https://s3.amazonaws.com/cf-templates-ozkq9d3hgiq2-us-east-1/..."@ 
  , description :: Core.Maybe Types.ProvisioningArtifactDescription
    -- ^ The description of the provisioning artifact, including how it differs from the previous provisioning artifact.
  , disableTemplateValidation :: Core.Maybe Core.Bool
    -- ^ If set to true, AWS Service Catalog stops validating the specified provisioning artifact even if it is invalid.
  , name :: Core.Maybe Types.ProvisioningArtifactName
    -- ^ The name of the provisioning artifact (for example, v1 v2beta). No spaces are allowed.
  , type' :: Core.Maybe Types.ProvisioningArtifactType
    -- ^ The type of provisioning artifact.
--
--
--     * @CLOUD_FORMATION_TEMPLATE@ - AWS CloudFormation template
--
--
--     * @MARKETPLACE_AMI@ - AWS Marketplace AMI
--
--
--     * @MARKETPLACE_CAR@ - AWS Marketplace Clusters and AWS Resources
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ProvisioningArtifactProperties' value with any optional fields omitted.
mkProvisioningArtifactProperties
    :: ProvisioningArtifactProperties
mkProvisioningArtifactProperties
  = ProvisioningArtifactProperties'{info = Core.mempty,
                                    description = Core.Nothing,
                                    disableTemplateValidation = Core.Nothing, name = Core.Nothing,
                                    type' = Core.Nothing}

-- | The URL of the CloudFormation template in Amazon S3. Specify the URL in JSON format as follows:
--
-- @"LoadTemplateFromURL": "https://s3.amazonaws.com/cf-templates-ozkq9d3hgiq2-us-east-1/..."@ 
--
-- /Note:/ Consider using 'info' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pInfo :: Lens.Lens' ProvisioningArtifactProperties (Core.HashMap Types.ProvisioningArtifactInfoKey Types.ProvisioningArtifactInfoValue)
pInfo = Lens.field @"info"
{-# INLINEABLE pInfo #-}
{-# DEPRECATED info "Use generic-lens or generic-optics with 'info' instead"  #-}

-- | The description of the provisioning artifact, including how it differs from the previous provisioning artifact.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pDescription :: Lens.Lens' ProvisioningArtifactProperties (Core.Maybe Types.ProvisioningArtifactDescription)
pDescription = Lens.field @"description"
{-# INLINEABLE pDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | If set to true, AWS Service Catalog stops validating the specified provisioning artifact even if it is invalid.
--
-- /Note:/ Consider using 'disableTemplateValidation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pDisableTemplateValidation :: Lens.Lens' ProvisioningArtifactProperties (Core.Maybe Core.Bool)
pDisableTemplateValidation = Lens.field @"disableTemplateValidation"
{-# INLINEABLE pDisableTemplateValidation #-}
{-# DEPRECATED disableTemplateValidation "Use generic-lens or generic-optics with 'disableTemplateValidation' instead"  #-}

-- | The name of the provisioning artifact (for example, v1 v2beta). No spaces are allowed.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pName :: Lens.Lens' ProvisioningArtifactProperties (Core.Maybe Types.ProvisioningArtifactName)
pName = Lens.field @"name"
{-# INLINEABLE pName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The type of provisioning artifact.
--
--
--     * @CLOUD_FORMATION_TEMPLATE@ - AWS CloudFormation template
--
--
--     * @MARKETPLACE_AMI@ - AWS Marketplace AMI
--
--
--     * @MARKETPLACE_CAR@ - AWS Marketplace Clusters and AWS Resources
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pType :: Lens.Lens' ProvisioningArtifactProperties (Core.Maybe Types.ProvisioningArtifactType)
pType = Lens.field @"type'"
{-# INLINEABLE pType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON ProvisioningArtifactProperties where
        toJSON ProvisioningArtifactProperties{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Info" Core..= info),
                  ("Description" Core..=) Core.<$> description,
                  ("DisableTemplateValidation" Core..=) Core.<$>
                    disableTemplateValidation,
                  ("Name" Core..=) Core.<$> name, ("Type" Core..=) Core.<$> type'])
