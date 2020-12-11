-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.MetadataInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.MetadataInfo
  ( MetadataInfo (..),

    -- * Smart constructor
    mkMetadataInfo,

    -- * Lenses
    miCreatedTime,
    miMetadataValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A structure containing metadata information for a schema version.
--
-- /See:/ 'mkMetadataInfo' smart constructor.
data MetadataInfo = MetadataInfo'
  { createdTime ::
      Lude.Maybe Lude.Text,
    metadataValue :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MetadataInfo' with the minimum fields required to make a request.
--
-- * 'createdTime' - The time at which the entry was created.
-- * 'metadataValue' - The metadata key’s corresponding value.
mkMetadataInfo ::
  MetadataInfo
mkMetadataInfo =
  MetadataInfo'
    { createdTime = Lude.Nothing,
      metadataValue = Lude.Nothing
    }

-- | The time at which the entry was created.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miCreatedTime :: Lens.Lens' MetadataInfo (Lude.Maybe Lude.Text)
miCreatedTime = Lens.lens (createdTime :: MetadataInfo -> Lude.Maybe Lude.Text) (\s a -> s {createdTime = a} :: MetadataInfo)
{-# DEPRECATED miCreatedTime "Use generic-lens or generic-optics with 'createdTime' instead." #-}

-- | The metadata key’s corresponding value.
--
-- /Note:/ Consider using 'metadataValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miMetadataValue :: Lens.Lens' MetadataInfo (Lude.Maybe Lude.Text)
miMetadataValue = Lens.lens (metadataValue :: MetadataInfo -> Lude.Maybe Lude.Text) (\s a -> s {metadataValue = a} :: MetadataInfo)
{-# DEPRECATED miMetadataValue "Use generic-lens or generic-optics with 'metadataValue' instead." #-}

instance Lude.FromJSON MetadataInfo where
  parseJSON =
    Lude.withObject
      "MetadataInfo"
      ( \x ->
          MetadataInfo'
            Lude.<$> (x Lude..:? "CreatedTime") Lude.<*> (x Lude..:? "MetadataValue")
      )
