{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.MetadataEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.MetadataEntry
  ( MetadataEntry (..),

    -- * Smart constructor
    mkMetadataEntry,

    -- * Lenses
    meValue,
    meName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal

-- | A metadata key-value pair to store with an object.
--
-- /See:/ 'mkMetadataEntry' smart constructor.
data MetadataEntry = MetadataEntry'
  { -- | Value of the Object.
    value :: Lude.Maybe Lude.Text,
    -- | Name of the Object.
    name :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MetadataEntry' with the minimum fields required to make a request.
--
-- * 'value' - Value of the Object.
-- * 'name' - Name of the Object.
mkMetadataEntry ::
  MetadataEntry
mkMetadataEntry =
  MetadataEntry' {value = Lude.Nothing, name = Lude.Nothing}

-- | Value of the Object.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meValue :: Lens.Lens' MetadataEntry (Lude.Maybe Lude.Text)
meValue = Lens.lens (value :: MetadataEntry -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: MetadataEntry)
{-# DEPRECATED meValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | Name of the Object.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meName :: Lens.Lens' MetadataEntry (Lude.Maybe Lude.Text)
meName = Lens.lens (name :: MetadataEntry -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: MetadataEntry)
{-# DEPRECATED meName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.ToXML MetadataEntry where
  toXML MetadataEntry' {..} =
    Lude.mconcat ["Value" Lude.@= value, "Name" Lude.@= name]
