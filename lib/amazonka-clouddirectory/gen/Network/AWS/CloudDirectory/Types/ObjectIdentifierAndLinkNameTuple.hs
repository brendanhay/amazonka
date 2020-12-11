-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.ObjectIdentifierAndLinkNameTuple
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.ObjectIdentifierAndLinkNameTuple
  ( ObjectIdentifierAndLinkNameTuple (..),

    -- * Smart constructor
    mkObjectIdentifierAndLinkNameTuple,

    -- * Lenses
    oialntObjectIdentifier,
    oialntLinkName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A pair of ObjectIdentifier and LinkName.
--
-- /See:/ 'mkObjectIdentifierAndLinkNameTuple' smart constructor.
data ObjectIdentifierAndLinkNameTuple = ObjectIdentifierAndLinkNameTuple'
  { objectIdentifier ::
      Lude.Maybe Lude.Text,
    linkName ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ObjectIdentifierAndLinkNameTuple' with the minimum fields required to make a request.
--
-- * 'linkName' - The name of the link between the parent and the child object.
-- * 'objectIdentifier' - The ID that is associated with the object.
mkObjectIdentifierAndLinkNameTuple ::
  ObjectIdentifierAndLinkNameTuple
mkObjectIdentifierAndLinkNameTuple =
  ObjectIdentifierAndLinkNameTuple'
    { objectIdentifier =
        Lude.Nothing,
      linkName = Lude.Nothing
    }

-- | The ID that is associated with the object.
--
-- /Note:/ Consider using 'objectIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oialntObjectIdentifier :: Lens.Lens' ObjectIdentifierAndLinkNameTuple (Lude.Maybe Lude.Text)
oialntObjectIdentifier = Lens.lens (objectIdentifier :: ObjectIdentifierAndLinkNameTuple -> Lude.Maybe Lude.Text) (\s a -> s {objectIdentifier = a} :: ObjectIdentifierAndLinkNameTuple)
{-# DEPRECATED oialntObjectIdentifier "Use generic-lens or generic-optics with 'objectIdentifier' instead." #-}

-- | The name of the link between the parent and the child object.
--
-- /Note:/ Consider using 'linkName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oialntLinkName :: Lens.Lens' ObjectIdentifierAndLinkNameTuple (Lude.Maybe Lude.Text)
oialntLinkName = Lens.lens (linkName :: ObjectIdentifierAndLinkNameTuple -> Lude.Maybe Lude.Text) (\s a -> s {linkName = a} :: ObjectIdentifierAndLinkNameTuple)
{-# DEPRECATED oialntLinkName "Use generic-lens or generic-optics with 'linkName' instead." #-}

instance Lude.FromJSON ObjectIdentifierAndLinkNameTuple where
  parseJSON =
    Lude.withObject
      "ObjectIdentifierAndLinkNameTuple"
      ( \x ->
          ObjectIdentifierAndLinkNameTuple'
            Lude.<$> (x Lude..:? "ObjectIdentifier") Lude.<*> (x Lude..:? "LinkName")
      )
