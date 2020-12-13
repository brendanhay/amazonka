{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisioningArtifactSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProvisioningArtifactSummary
  ( ProvisioningArtifactSummary (..),

    -- * Smart constructor
    mkProvisioningArtifactSummary,

    -- * Lenses
    pasProvisioningArtifactMetadata,
    pasCreatedTime,
    pasName,
    pasId,
    pasDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Summary information about a provisioning artifact (also known as a version) for a product.
--
-- /See:/ 'mkProvisioningArtifactSummary' smart constructor.
data ProvisioningArtifactSummary = ProvisioningArtifactSummary'
  { -- | The metadata for the provisioning artifact. This is used with AWS Marketplace products.
    provisioningArtifactMetadata :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The UTC time stamp of the creation time.
    createdTime :: Lude.Maybe Lude.Timestamp,
    -- | The name of the provisioning artifact.
    name :: Lude.Maybe Lude.Text,
    -- | The identifier of the provisioning artifact.
    id :: Lude.Maybe Lude.Text,
    -- | The description of the provisioning artifact.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProvisioningArtifactSummary' with the minimum fields required to make a request.
--
-- * 'provisioningArtifactMetadata' - The metadata for the provisioning artifact. This is used with AWS Marketplace products.
-- * 'createdTime' - The UTC time stamp of the creation time.
-- * 'name' - The name of the provisioning artifact.
-- * 'id' - The identifier of the provisioning artifact.
-- * 'description' - The description of the provisioning artifact.
mkProvisioningArtifactSummary ::
  ProvisioningArtifactSummary
mkProvisioningArtifactSummary =
  ProvisioningArtifactSummary'
    { provisioningArtifactMetadata =
        Lude.Nothing,
      createdTime = Lude.Nothing,
      name = Lude.Nothing,
      id = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The metadata for the provisioning artifact. This is used with AWS Marketplace products.
--
-- /Note:/ Consider using 'provisioningArtifactMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pasProvisioningArtifactMetadata :: Lens.Lens' ProvisioningArtifactSummary (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
pasProvisioningArtifactMetadata = Lens.lens (provisioningArtifactMetadata :: ProvisioningArtifactSummary -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {provisioningArtifactMetadata = a} :: ProvisioningArtifactSummary)
{-# DEPRECATED pasProvisioningArtifactMetadata "Use generic-lens or generic-optics with 'provisioningArtifactMetadata' instead." #-}

-- | The UTC time stamp of the creation time.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pasCreatedTime :: Lens.Lens' ProvisioningArtifactSummary (Lude.Maybe Lude.Timestamp)
pasCreatedTime = Lens.lens (createdTime :: ProvisioningArtifactSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdTime = a} :: ProvisioningArtifactSummary)
{-# DEPRECATED pasCreatedTime "Use generic-lens or generic-optics with 'createdTime' instead." #-}

-- | The name of the provisioning artifact.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pasName :: Lens.Lens' ProvisioningArtifactSummary (Lude.Maybe Lude.Text)
pasName = Lens.lens (name :: ProvisioningArtifactSummary -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ProvisioningArtifactSummary)
{-# DEPRECATED pasName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The identifier of the provisioning artifact.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pasId :: Lens.Lens' ProvisioningArtifactSummary (Lude.Maybe Lude.Text)
pasId = Lens.lens (id :: ProvisioningArtifactSummary -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: ProvisioningArtifactSummary)
{-# DEPRECATED pasId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The description of the provisioning artifact.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pasDescription :: Lens.Lens' ProvisioningArtifactSummary (Lude.Maybe Lude.Text)
pasDescription = Lens.lens (description :: ProvisioningArtifactSummary -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ProvisioningArtifactSummary)
{-# DEPRECATED pasDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON ProvisioningArtifactSummary where
  parseJSON =
    Lude.withObject
      "ProvisioningArtifactSummary"
      ( \x ->
          ProvisioningArtifactSummary'
            Lude.<$> (x Lude..:? "ProvisioningArtifactMetadata" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "CreatedTime")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "Description")
      )
