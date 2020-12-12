{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.Difference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.Difference
  ( Difference (..),

    -- * Smart constructor
    mkDifference,

    -- * Lenses
    dAfterBlob,
    dBeforeBlob,
    dChangeType,
  )
where

import Network.AWS.CodeCommit.Types.BlobMetadata
import Network.AWS.CodeCommit.Types.ChangeTypeEnum
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Returns information about a set of differences for a commit specifier.
--
-- /See:/ 'mkDifference' smart constructor.
data Difference = Difference'
  { afterBlob :: Lude.Maybe BlobMetadata,
    beforeBlob :: Lude.Maybe BlobMetadata,
    changeType :: Lude.Maybe ChangeTypeEnum
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Difference' with the minimum fields required to make a request.
--
-- * 'afterBlob' - Information about an @afterBlob@ data type object, including the ID, the file mode permission code, and the path.
-- * 'beforeBlob' - Information about a @beforeBlob@ data type object, including the ID, the file mode permission code, and the path.
-- * 'changeType' - Whether the change type of the difference is an addition (A), deletion (D), or modification (M).
mkDifference ::
  Difference
mkDifference =
  Difference'
    { afterBlob = Lude.Nothing,
      beforeBlob = Lude.Nothing,
      changeType = Lude.Nothing
    }

-- | Information about an @afterBlob@ data type object, including the ID, the file mode permission code, and the path.
--
-- /Note:/ Consider using 'afterBlob' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAfterBlob :: Lens.Lens' Difference (Lude.Maybe BlobMetadata)
dAfterBlob = Lens.lens (afterBlob :: Difference -> Lude.Maybe BlobMetadata) (\s a -> s {afterBlob = a} :: Difference)
{-# DEPRECATED dAfterBlob "Use generic-lens or generic-optics with 'afterBlob' instead." #-}

-- | Information about a @beforeBlob@ data type object, including the ID, the file mode permission code, and the path.
--
-- /Note:/ Consider using 'beforeBlob' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dBeforeBlob :: Lens.Lens' Difference (Lude.Maybe BlobMetadata)
dBeforeBlob = Lens.lens (beforeBlob :: Difference -> Lude.Maybe BlobMetadata) (\s a -> s {beforeBlob = a} :: Difference)
{-# DEPRECATED dBeforeBlob "Use generic-lens or generic-optics with 'beforeBlob' instead." #-}

-- | Whether the change type of the difference is an addition (A), deletion (D), or modification (M).
--
-- /Note:/ Consider using 'changeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dChangeType :: Lens.Lens' Difference (Lude.Maybe ChangeTypeEnum)
dChangeType = Lens.lens (changeType :: Difference -> Lude.Maybe ChangeTypeEnum) (\s a -> s {changeType = a} :: Difference)
{-# DEPRECATED dChangeType "Use generic-lens or generic-optics with 'changeType' instead." #-}

instance Lude.FromJSON Difference where
  parseJSON =
    Lude.withObject
      "Difference"
      ( \x ->
          Difference'
            Lude.<$> (x Lude..:? "afterBlob")
            Lude.<*> (x Lude..:? "beforeBlob")
            Lude.<*> (x Lude..:? "changeType")
      )
