{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisioningArtifactDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProvisioningArtifactDetail
  ( ProvisioningArtifactDetail (..),

    -- * Smart constructor
    mkProvisioningArtifactDetail,

    -- * Lenses
    padCreatedTime,
    padActive,
    padName,
    padId,
    padType,
    padGuidance,
    padDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.ServiceCatalog.Types.ProvisioningArtifactGuidance
import Network.AWS.ServiceCatalog.Types.ProvisioningArtifactType

-- | Information about a provisioning artifact (also known as a version) for a product.
--
-- /See:/ 'mkProvisioningArtifactDetail' smart constructor.
data ProvisioningArtifactDetail = ProvisioningArtifactDetail'
  { -- | The UTC time stamp of the creation time.
    createdTime :: Lude.Maybe Lude.Timestamp,
    -- | Indicates whether the product version is active.
    active :: Lude.Maybe Lude.Bool,
    -- | The name of the provisioning artifact.
    name :: Lude.Maybe Lude.Text,
    -- | The identifier of the provisioning artifact.
    id :: Lude.Maybe Lude.Text,
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
    type' :: Lude.Maybe ProvisioningArtifactType,
    -- | Information set by the administrator to provide guidance to end users about which provisioning artifacts to use.
    guidance :: Lude.Maybe ProvisioningArtifactGuidance,
    -- | The description of the provisioning artifact.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProvisioningArtifactDetail' with the minimum fields required to make a request.
--
-- * 'createdTime' - The UTC time stamp of the creation time.
-- * 'active' - Indicates whether the product version is active.
-- * 'name' - The name of the provisioning artifact.
-- * 'id' - The identifier of the provisioning artifact.
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
--
--
-- * 'guidance' - Information set by the administrator to provide guidance to end users about which provisioning artifacts to use.
-- * 'description' - The description of the provisioning artifact.
mkProvisioningArtifactDetail ::
  ProvisioningArtifactDetail
mkProvisioningArtifactDetail =
  ProvisioningArtifactDetail'
    { createdTime = Lude.Nothing,
      active = Lude.Nothing,
      name = Lude.Nothing,
      id = Lude.Nothing,
      type' = Lude.Nothing,
      guidance = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The UTC time stamp of the creation time.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
padCreatedTime :: Lens.Lens' ProvisioningArtifactDetail (Lude.Maybe Lude.Timestamp)
padCreatedTime = Lens.lens (createdTime :: ProvisioningArtifactDetail -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdTime = a} :: ProvisioningArtifactDetail)
{-# DEPRECATED padCreatedTime "Use generic-lens or generic-optics with 'createdTime' instead." #-}

-- | Indicates whether the product version is active.
--
-- /Note:/ Consider using 'active' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
padActive :: Lens.Lens' ProvisioningArtifactDetail (Lude.Maybe Lude.Bool)
padActive = Lens.lens (active :: ProvisioningArtifactDetail -> Lude.Maybe Lude.Bool) (\s a -> s {active = a} :: ProvisioningArtifactDetail)
{-# DEPRECATED padActive "Use generic-lens or generic-optics with 'active' instead." #-}

-- | The name of the provisioning artifact.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
padName :: Lens.Lens' ProvisioningArtifactDetail (Lude.Maybe Lude.Text)
padName = Lens.lens (name :: ProvisioningArtifactDetail -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ProvisioningArtifactDetail)
{-# DEPRECATED padName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The identifier of the provisioning artifact.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
padId :: Lens.Lens' ProvisioningArtifactDetail (Lude.Maybe Lude.Text)
padId = Lens.lens (id :: ProvisioningArtifactDetail -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: ProvisioningArtifactDetail)
{-# DEPRECATED padId "Use generic-lens or generic-optics with 'id' instead." #-}

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
padType :: Lens.Lens' ProvisioningArtifactDetail (Lude.Maybe ProvisioningArtifactType)
padType = Lens.lens (type' :: ProvisioningArtifactDetail -> Lude.Maybe ProvisioningArtifactType) (\s a -> s {type' = a} :: ProvisioningArtifactDetail)
{-# DEPRECATED padType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | Information set by the administrator to provide guidance to end users about which provisioning artifacts to use.
--
-- /Note:/ Consider using 'guidance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
padGuidance :: Lens.Lens' ProvisioningArtifactDetail (Lude.Maybe ProvisioningArtifactGuidance)
padGuidance = Lens.lens (guidance :: ProvisioningArtifactDetail -> Lude.Maybe ProvisioningArtifactGuidance) (\s a -> s {guidance = a} :: ProvisioningArtifactDetail)
{-# DEPRECATED padGuidance "Use generic-lens or generic-optics with 'guidance' instead." #-}

-- | The description of the provisioning artifact.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
padDescription :: Lens.Lens' ProvisioningArtifactDetail (Lude.Maybe Lude.Text)
padDescription = Lens.lens (description :: ProvisioningArtifactDetail -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ProvisioningArtifactDetail)
{-# DEPRECATED padDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON ProvisioningArtifactDetail where
  parseJSON =
    Lude.withObject
      "ProvisioningArtifactDetail"
      ( \x ->
          ProvisioningArtifactDetail'
            Lude.<$> (x Lude..:? "CreatedTime")
            Lude.<*> (x Lude..:? "Active")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "Type")
            Lude.<*> (x Lude..:? "Guidance")
            Lude.<*> (x Lude..:? "Description")
      )
