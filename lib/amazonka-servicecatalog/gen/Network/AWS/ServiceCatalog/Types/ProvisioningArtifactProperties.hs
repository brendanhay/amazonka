-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisioningArtifactProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProvisioningArtifactProperties
  ( ProvisioningArtifactProperties (..),

    -- * Smart constructor
    mkProvisioningArtifactProperties,

    -- * Lenses
    papDisableTemplateValidation,
    papName,
    papType,
    papDescription,
    papInfo,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.ServiceCatalog.Types.ProvisioningArtifactType

-- | Information about a provisioning artifact (also known as a version) for a product.
--
-- /See:/ 'mkProvisioningArtifactProperties' smart constructor.
data ProvisioningArtifactProperties = ProvisioningArtifactProperties'
  { disableTemplateValidation ::
      Lude.Maybe Lude.Bool,
    name :: Lude.Maybe Lude.Text,
    type' ::
      Lude.Maybe
        ProvisioningArtifactType,
    description ::
      Lude.Maybe Lude.Text,
    info ::
      Lude.HashMap
        Lude.Text
        (Lude.Text)
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProvisioningArtifactProperties' with the minimum fields required to make a request.
--
-- * 'description' - The description of the provisioning artifact, including how it differs from the previous provisioning artifact.
-- * 'disableTemplateValidation' - If set to true, AWS Service Catalog stops validating the specified provisioning artifact even if it is invalid.
-- * 'info' - The URL of the CloudFormation template in Amazon S3. Specify the URL in JSON format as follows:
--
-- @"LoadTemplateFromURL": "https://s3.amazonaws.com/cf-templates-ozkq9d3hgiq2-us-east-1/..."@
-- * 'name' - The name of the provisioning artifact (for example, v1 v2beta). No spaces are allowed.
-- * 'type'' - The type of provisioning artifact.
--
--
--     * @CLOUD_FORMATION_TEMPLATE@ - AWS CloudFormation template
--
--
--     * @MARKETPLACE_AMI@ - AWS Marketplace AMI
--
--
--     * @MARKETPLACE_CAR@ - AWS Marketplace Clusters and AWS Resources
mkProvisioningArtifactProperties ::
  ProvisioningArtifactProperties
mkProvisioningArtifactProperties =
  ProvisioningArtifactProperties'
    { disableTemplateValidation =
        Lude.Nothing,
      name = Lude.Nothing,
      type' = Lude.Nothing,
      description = Lude.Nothing,
      info = Lude.mempty
    }

-- | If set to true, AWS Service Catalog stops validating the specified provisioning artifact even if it is invalid.
--
-- /Note:/ Consider using 'disableTemplateValidation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
papDisableTemplateValidation :: Lens.Lens' ProvisioningArtifactProperties (Lude.Maybe Lude.Bool)
papDisableTemplateValidation = Lens.lens (disableTemplateValidation :: ProvisioningArtifactProperties -> Lude.Maybe Lude.Bool) (\s a -> s {disableTemplateValidation = a} :: ProvisioningArtifactProperties)
{-# DEPRECATED papDisableTemplateValidation "Use generic-lens or generic-optics with 'disableTemplateValidation' instead." #-}

-- | The name of the provisioning artifact (for example, v1 v2beta). No spaces are allowed.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
papName :: Lens.Lens' ProvisioningArtifactProperties (Lude.Maybe Lude.Text)
papName = Lens.lens (name :: ProvisioningArtifactProperties -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ProvisioningArtifactProperties)
{-# DEPRECATED papName "Use generic-lens or generic-optics with 'name' instead." #-}

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
papType :: Lens.Lens' ProvisioningArtifactProperties (Lude.Maybe ProvisioningArtifactType)
papType = Lens.lens (type' :: ProvisioningArtifactProperties -> Lude.Maybe ProvisioningArtifactType) (\s a -> s {type' = a} :: ProvisioningArtifactProperties)
{-# DEPRECATED papType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The description of the provisioning artifact, including how it differs from the previous provisioning artifact.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
papDescription :: Lens.Lens' ProvisioningArtifactProperties (Lude.Maybe Lude.Text)
papDescription = Lens.lens (description :: ProvisioningArtifactProperties -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ProvisioningArtifactProperties)
{-# DEPRECATED papDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The URL of the CloudFormation template in Amazon S3. Specify the URL in JSON format as follows:
--
-- @"LoadTemplateFromURL": "https://s3.amazonaws.com/cf-templates-ozkq9d3hgiq2-us-east-1/..."@
--
-- /Note:/ Consider using 'info' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
papInfo :: Lens.Lens' ProvisioningArtifactProperties (Lude.HashMap Lude.Text (Lude.Text))
papInfo = Lens.lens (info :: ProvisioningArtifactProperties -> Lude.HashMap Lude.Text (Lude.Text)) (\s a -> s {info = a} :: ProvisioningArtifactProperties)
{-# DEPRECATED papInfo "Use generic-lens or generic-optics with 'info' instead." #-}

instance Lude.ToJSON ProvisioningArtifactProperties where
  toJSON ProvisioningArtifactProperties' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DisableTemplateValidation" Lude..=)
              Lude.<$> disableTemplateValidation,
            ("Name" Lude..=) Lude.<$> name,
            ("Type" Lude..=) Lude.<$> type',
            ("Description" Lude..=) Lude.<$> description,
            Lude.Just ("Info" Lude..= info)
          ]
      )
