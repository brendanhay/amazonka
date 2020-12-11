-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.HSMConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.HSMConfiguration
  ( HSMConfiguration (..),

    -- * Smart constructor
    mkHSMConfiguration,

    -- * Lenses
    hcHSMConfigurationIdentifier,
    hcHSMPartitionName,
    hcDescription,
    hcTags,
    hcHSMIPAddress,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.Tag

-- | Returns information about an HSM configuration, which is an object that describes to Amazon Redshift clusters the information they require to connect to an HSM where they can store database encryption keys.
--
-- /See:/ 'mkHSMConfiguration' smart constructor.
data HSMConfiguration = HSMConfiguration'
  { hsmConfigurationIdentifier ::
      Lude.Maybe Lude.Text,
    hsmPartitionName :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    hsmIPAddress :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HSMConfiguration' with the minimum fields required to make a request.
--
-- * 'description' - A text description of the HSM configuration.
-- * 'hsmConfigurationIdentifier' - The name of the Amazon Redshift HSM configuration.
-- * 'hsmIPAddress' - The IP address that the Amazon Redshift cluster must use to access the HSM.
-- * 'hsmPartitionName' - The name of the partition in the HSM where the Amazon Redshift clusters will store their database encryption keys.
-- * 'tags' - The list of tags for the HSM configuration.
mkHSMConfiguration ::
  HSMConfiguration
mkHSMConfiguration =
  HSMConfiguration'
    { hsmConfigurationIdentifier = Lude.Nothing,
      hsmPartitionName = Lude.Nothing,
      description = Lude.Nothing,
      tags = Lude.Nothing,
      hsmIPAddress = Lude.Nothing
    }

-- | The name of the Amazon Redshift HSM configuration.
--
-- /Note:/ Consider using 'hsmConfigurationIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcHSMConfigurationIdentifier :: Lens.Lens' HSMConfiguration (Lude.Maybe Lude.Text)
hcHSMConfigurationIdentifier = Lens.lens (hsmConfigurationIdentifier :: HSMConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {hsmConfigurationIdentifier = a} :: HSMConfiguration)
{-# DEPRECATED hcHSMConfigurationIdentifier "Use generic-lens or generic-optics with 'hsmConfigurationIdentifier' instead." #-}

-- | The name of the partition in the HSM where the Amazon Redshift clusters will store their database encryption keys.
--
-- /Note:/ Consider using 'hsmPartitionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcHSMPartitionName :: Lens.Lens' HSMConfiguration (Lude.Maybe Lude.Text)
hcHSMPartitionName = Lens.lens (hsmPartitionName :: HSMConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {hsmPartitionName = a} :: HSMConfiguration)
{-# DEPRECATED hcHSMPartitionName "Use generic-lens or generic-optics with 'hsmPartitionName' instead." #-}

-- | A text description of the HSM configuration.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcDescription :: Lens.Lens' HSMConfiguration (Lude.Maybe Lude.Text)
hcDescription = Lens.lens (description :: HSMConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: HSMConfiguration)
{-# DEPRECATED hcDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The list of tags for the HSM configuration.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcTags :: Lens.Lens' HSMConfiguration (Lude.Maybe [Tag])
hcTags = Lens.lens (tags :: HSMConfiguration -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: HSMConfiguration)
{-# DEPRECATED hcTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The IP address that the Amazon Redshift cluster must use to access the HSM.
--
-- /Note:/ Consider using 'hsmIPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hcHSMIPAddress :: Lens.Lens' HSMConfiguration (Lude.Maybe Lude.Text)
hcHSMIPAddress = Lens.lens (hsmIPAddress :: HSMConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {hsmIPAddress = a} :: HSMConfiguration)
{-# DEPRECATED hcHSMIPAddress "Use generic-lens or generic-optics with 'hsmIPAddress' instead." #-}

instance Lude.FromXML HSMConfiguration where
  parseXML x =
    HSMConfiguration'
      Lude.<$> (x Lude..@? "HsmConfigurationIdentifier")
      Lude.<*> (x Lude..@? "HsmPartitionName")
      Lude.<*> (x Lude..@? "Description")
      Lude.<*> ( x Lude..@? "Tags" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "Tag")
               )
      Lude.<*> (x Lude..@? "HsmIpAddress")
