{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.MetadataKeyValuePair
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.MetadataKeyValuePair
  ( MetadataKeyValuePair (..),

    -- * Smart constructor
    mkMetadataKeyValuePair,

    -- * Lenses
    mkvpMetadataKey,
    mkvpMetadataValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A structure containing a key value pair for metadata.
--
-- /See:/ 'mkMetadataKeyValuePair' smart constructor.
data MetadataKeyValuePair = MetadataKeyValuePair'
  { -- | A metadata key.
    metadataKey :: Lude.Maybe Lude.Text,
    -- | A metadata key’s corresponding value.
    metadataValue :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MetadataKeyValuePair' with the minimum fields required to make a request.
--
-- * 'metadataKey' - A metadata key.
-- * 'metadataValue' - A metadata key’s corresponding value.
mkMetadataKeyValuePair ::
  MetadataKeyValuePair
mkMetadataKeyValuePair =
  MetadataKeyValuePair'
    { metadataKey = Lude.Nothing,
      metadataValue = Lude.Nothing
    }

-- | A metadata key.
--
-- /Note:/ Consider using 'metadataKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mkvpMetadataKey :: Lens.Lens' MetadataKeyValuePair (Lude.Maybe Lude.Text)
mkvpMetadataKey = Lens.lens (metadataKey :: MetadataKeyValuePair -> Lude.Maybe Lude.Text) (\s a -> s {metadataKey = a} :: MetadataKeyValuePair)
{-# DEPRECATED mkvpMetadataKey "Use generic-lens or generic-optics with 'metadataKey' instead." #-}

-- | A metadata key’s corresponding value.
--
-- /Note:/ Consider using 'metadataValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mkvpMetadataValue :: Lens.Lens' MetadataKeyValuePair (Lude.Maybe Lude.Text)
mkvpMetadataValue = Lens.lens (metadataValue :: MetadataKeyValuePair -> Lude.Maybe Lude.Text) (\s a -> s {metadataValue = a} :: MetadataKeyValuePair)
{-# DEPRECATED mkvpMetadataValue "Use generic-lens or generic-optics with 'metadataValue' instead." #-}

instance Lude.ToJSON MetadataKeyValuePair where
  toJSON MetadataKeyValuePair' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("MetadataKey" Lude..=) Lude.<$> metadataKey,
            ("MetadataValue" Lude..=) Lude.<$> metadataValue
          ]
      )
