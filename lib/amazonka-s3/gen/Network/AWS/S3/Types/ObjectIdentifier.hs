{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ObjectIdentifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ObjectIdentifier
  ( ObjectIdentifier (..),

    -- * Smart constructor
    mkObjectIdentifier,

    -- * Lenses
    oiVersionId,
    oiKey,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal

-- | Object Identifier is unique value to identify objects.
--
-- /See:/ 'mkObjectIdentifier' smart constructor.
data ObjectIdentifier = ObjectIdentifier'
  { -- | VersionId for the specific version of the object to delete.
    versionId :: Lude.Maybe ObjectVersionId,
    -- | Key name of the object to delete.
    key :: ObjectKey
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ObjectIdentifier' with the minimum fields required to make a request.
--
-- * 'versionId' - VersionId for the specific version of the object to delete.
-- * 'key' - Key name of the object to delete.
mkObjectIdentifier ::
  -- | 'key'
  ObjectKey ->
  ObjectIdentifier
mkObjectIdentifier pKey_ =
  ObjectIdentifier' {versionId = Lude.Nothing, key = pKey_}

-- | VersionId for the specific version of the object to delete.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oiVersionId :: Lens.Lens' ObjectIdentifier (Lude.Maybe ObjectVersionId)
oiVersionId = Lens.lens (versionId :: ObjectIdentifier -> Lude.Maybe ObjectVersionId) (\s a -> s {versionId = a} :: ObjectIdentifier)
{-# DEPRECATED oiVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | Key name of the object to delete.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oiKey :: Lens.Lens' ObjectIdentifier ObjectKey
oiKey = Lens.lens (key :: ObjectIdentifier -> ObjectKey) (\s a -> s {key = a} :: ObjectIdentifier)
{-# DEPRECATED oiKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.ToXML ObjectIdentifier where
  toXML ObjectIdentifier' {..} =
    Lude.mconcat ["VersionId" Lude.@= versionId, "Key" Lude.@= key]
