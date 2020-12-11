-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisioningArtifactOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProvisioningArtifactOutput
  ( ProvisioningArtifactOutput (..),

    -- * Smart constructor
    mkProvisioningArtifactOutput,

    -- * Lenses
    paoKey,
    paoDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provisioning artifact output.
--
-- /See:/ 'mkProvisioningArtifactOutput' smart constructor.
data ProvisioningArtifactOutput = ProvisioningArtifactOutput'
  { key ::
      Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProvisioningArtifactOutput' with the minimum fields required to make a request.
--
-- * 'description' - Description of the provisioning artifact output key.
-- * 'key' - The provisioning artifact output key.
mkProvisioningArtifactOutput ::
  ProvisioningArtifactOutput
mkProvisioningArtifactOutput =
  ProvisioningArtifactOutput'
    { key = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The provisioning artifact output key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paoKey :: Lens.Lens' ProvisioningArtifactOutput (Lude.Maybe Lude.Text)
paoKey = Lens.lens (key :: ProvisioningArtifactOutput -> Lude.Maybe Lude.Text) (\s a -> s {key = a} :: ProvisioningArtifactOutput)
{-# DEPRECATED paoKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | Description of the provisioning artifact output key.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paoDescription :: Lens.Lens' ProvisioningArtifactOutput (Lude.Maybe Lude.Text)
paoDescription = Lens.lens (description :: ProvisioningArtifactOutput -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ProvisioningArtifactOutput)
{-# DEPRECATED paoDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON ProvisioningArtifactOutput where
  parseJSON =
    Lude.withObject
      "ProvisioningArtifactOutput"
      ( \x ->
          ProvisioningArtifactOutput'
            Lude.<$> (x Lude..:? "Key") Lude.<*> (x Lude..:? "Description")
      )
