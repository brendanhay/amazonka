{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.BlockPublicAccessConfigurationMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.BlockPublicAccessConfigurationMetadata
  ( BlockPublicAccessConfigurationMetadata (..),

    -- * Smart constructor
    mkBlockPublicAccessConfigurationMetadata,

    -- * Lenses
    bpacmCreatedByARN,
    bpacmCreationDateTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Properties that describe the AWS principal that created the @BlockPublicAccessConfiguration@ using the @PutBlockPublicAccessConfiguration@ action as well as the date and time that the configuration was created. Each time a configuration for block public access is updated, Amazon EMR updates this metadata.
--
-- /See:/ 'mkBlockPublicAccessConfigurationMetadata' smart constructor.
data BlockPublicAccessConfigurationMetadata = BlockPublicAccessConfigurationMetadata'
  { -- | The Amazon Resource Name that created or last modified the configuration.
    createdByARN :: Lude.Text,
    -- | The date and time that the configuration was created.
    creationDateTime :: Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BlockPublicAccessConfigurationMetadata' with the minimum fields required to make a request.
--
-- * 'createdByARN' - The Amazon Resource Name that created or last modified the configuration.
-- * 'creationDateTime' - The date and time that the configuration was created.
mkBlockPublicAccessConfigurationMetadata ::
  -- | 'createdByARN'
  Lude.Text ->
  -- | 'creationDateTime'
  Lude.Timestamp ->
  BlockPublicAccessConfigurationMetadata
mkBlockPublicAccessConfigurationMetadata
  pCreatedByARN_
  pCreationDateTime_ =
    BlockPublicAccessConfigurationMetadata'
      { createdByARN =
          pCreatedByARN_,
        creationDateTime = pCreationDateTime_
      }

-- | The Amazon Resource Name that created or last modified the configuration.
--
-- /Note:/ Consider using 'createdByARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpacmCreatedByARN :: Lens.Lens' BlockPublicAccessConfigurationMetadata Lude.Text
bpacmCreatedByARN = Lens.lens (createdByARN :: BlockPublicAccessConfigurationMetadata -> Lude.Text) (\s a -> s {createdByARN = a} :: BlockPublicAccessConfigurationMetadata)
{-# DEPRECATED bpacmCreatedByARN "Use generic-lens or generic-optics with 'createdByARN' instead." #-}

-- | The date and time that the configuration was created.
--
-- /Note:/ Consider using 'creationDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpacmCreationDateTime :: Lens.Lens' BlockPublicAccessConfigurationMetadata Lude.Timestamp
bpacmCreationDateTime = Lens.lens (creationDateTime :: BlockPublicAccessConfigurationMetadata -> Lude.Timestamp) (\s a -> s {creationDateTime = a} :: BlockPublicAccessConfigurationMetadata)
{-# DEPRECATED bpacmCreationDateTime "Use generic-lens or generic-optics with 'creationDateTime' instead." #-}

instance Lude.FromJSON BlockPublicAccessConfigurationMetadata where
  parseJSON =
    Lude.withObject
      "BlockPublicAccessConfigurationMetadata"
      ( \x ->
          BlockPublicAccessConfigurationMetadata'
            Lude.<$> (x Lude..: "CreatedByArn") Lude.<*> (x Lude..: "CreationDateTime")
      )
