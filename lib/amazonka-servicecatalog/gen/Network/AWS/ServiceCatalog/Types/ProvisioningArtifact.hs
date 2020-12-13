{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisioningArtifact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProvisioningArtifact
  ( ProvisioningArtifact (..),

    -- * Smart constructor
    mkProvisioningArtifact,

    -- * Lenses
    paCreatedTime,
    paName,
    paId,
    paGuidance,
    paDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.ServiceCatalog.Types.ProvisioningArtifactGuidance

-- | Information about a provisioning artifact. A provisioning artifact is also known as a product version.
--
-- /See:/ 'mkProvisioningArtifact' smart constructor.
data ProvisioningArtifact = ProvisioningArtifact'
  { -- | The UTC time stamp of the creation time.
    createdTime :: Lude.Maybe Lude.Timestamp,
    -- | The name of the provisioning artifact.
    name :: Lude.Maybe Lude.Text,
    -- | The identifier of the provisioning artifact.
    id :: Lude.Maybe Lude.Text,
    -- | Information set by the administrator to provide guidance to end users about which provisioning artifacts to use.
    guidance :: Lude.Maybe ProvisioningArtifactGuidance,
    -- | The description of the provisioning artifact.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProvisioningArtifact' with the minimum fields required to make a request.
--
-- * 'createdTime' - The UTC time stamp of the creation time.
-- * 'name' - The name of the provisioning artifact.
-- * 'id' - The identifier of the provisioning artifact.
-- * 'guidance' - Information set by the administrator to provide guidance to end users about which provisioning artifacts to use.
-- * 'description' - The description of the provisioning artifact.
mkProvisioningArtifact ::
  ProvisioningArtifact
mkProvisioningArtifact =
  ProvisioningArtifact'
    { createdTime = Lude.Nothing,
      name = Lude.Nothing,
      id = Lude.Nothing,
      guidance = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The UTC time stamp of the creation time.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paCreatedTime :: Lens.Lens' ProvisioningArtifact (Lude.Maybe Lude.Timestamp)
paCreatedTime = Lens.lens (createdTime :: ProvisioningArtifact -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdTime = a} :: ProvisioningArtifact)
{-# DEPRECATED paCreatedTime "Use generic-lens or generic-optics with 'createdTime' instead." #-}

-- | The name of the provisioning artifact.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paName :: Lens.Lens' ProvisioningArtifact (Lude.Maybe Lude.Text)
paName = Lens.lens (name :: ProvisioningArtifact -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ProvisioningArtifact)
{-# DEPRECATED paName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The identifier of the provisioning artifact.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paId :: Lens.Lens' ProvisioningArtifact (Lude.Maybe Lude.Text)
paId = Lens.lens (id :: ProvisioningArtifact -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: ProvisioningArtifact)
{-# DEPRECATED paId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Information set by the administrator to provide guidance to end users about which provisioning artifacts to use.
--
-- /Note:/ Consider using 'guidance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paGuidance :: Lens.Lens' ProvisioningArtifact (Lude.Maybe ProvisioningArtifactGuidance)
paGuidance = Lens.lens (guidance :: ProvisioningArtifact -> Lude.Maybe ProvisioningArtifactGuidance) (\s a -> s {guidance = a} :: ProvisioningArtifact)
{-# DEPRECATED paGuidance "Use generic-lens or generic-optics with 'guidance' instead." #-}

-- | The description of the provisioning artifact.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paDescription :: Lens.Lens' ProvisioningArtifact (Lude.Maybe Lude.Text)
paDescription = Lens.lens (description :: ProvisioningArtifact -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ProvisioningArtifact)
{-# DEPRECATED paDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON ProvisioningArtifact where
  parseJSON =
    Lude.withObject
      "ProvisioningArtifact"
      ( \x ->
          ProvisioningArtifact'
            Lude.<$> (x Lude..:? "CreatedTime")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "Guidance")
            Lude.<*> (x Lude..:? "Description")
      )
