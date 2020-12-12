{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.PathToObjectIdentifiers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.PathToObjectIdentifiers
  ( PathToObjectIdentifiers (..),

    -- * Smart constructor
    mkPathToObjectIdentifiers,

    -- * Lenses
    ptoiObjectIdentifiers,
    ptoiPath,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Returns the path to the @ObjectIdentifiers@ that is associated with the directory.
--
-- /See:/ 'mkPathToObjectIdentifiers' smart constructor.
data PathToObjectIdentifiers = PathToObjectIdentifiers'
  { objectIdentifiers ::
      Lude.Maybe [Lude.Text],
    path :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PathToObjectIdentifiers' with the minimum fields required to make a request.
--
-- * 'objectIdentifiers' - Lists @ObjectIdentifiers@ starting from directory root to the object in the request.
-- * 'path' - The path that is used to identify the object starting from directory root.
mkPathToObjectIdentifiers ::
  PathToObjectIdentifiers
mkPathToObjectIdentifiers =
  PathToObjectIdentifiers'
    { objectIdentifiers = Lude.Nothing,
      path = Lude.Nothing
    }

-- | Lists @ObjectIdentifiers@ starting from directory root to the object in the request.
--
-- /Note:/ Consider using 'objectIdentifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptoiObjectIdentifiers :: Lens.Lens' PathToObjectIdentifiers (Lude.Maybe [Lude.Text])
ptoiObjectIdentifiers = Lens.lens (objectIdentifiers :: PathToObjectIdentifiers -> Lude.Maybe [Lude.Text]) (\s a -> s {objectIdentifiers = a} :: PathToObjectIdentifiers)
{-# DEPRECATED ptoiObjectIdentifiers "Use generic-lens or generic-optics with 'objectIdentifiers' instead." #-}

-- | The path that is used to identify the object starting from directory root.
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptoiPath :: Lens.Lens' PathToObjectIdentifiers (Lude.Maybe Lude.Text)
ptoiPath = Lens.lens (path :: PathToObjectIdentifiers -> Lude.Maybe Lude.Text) (\s a -> s {path = a} :: PathToObjectIdentifiers)
{-# DEPRECATED ptoiPath "Use generic-lens or generic-optics with 'path' instead." #-}

instance Lude.FromJSON PathToObjectIdentifiers where
  parseJSON =
    Lude.withObject
      "PathToObjectIdentifiers"
      ( \x ->
          PathToObjectIdentifiers'
            Lude.<$> (x Lude..:? "ObjectIdentifiers" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Path")
      )
