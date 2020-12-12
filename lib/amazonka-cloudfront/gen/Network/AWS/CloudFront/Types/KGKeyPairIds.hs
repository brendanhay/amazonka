{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.KGKeyPairIds
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.KGKeyPairIds
  ( KGKeyPairIds (..),

    -- * Smart constructor
    mkKGKeyPairIds,

    -- * Lenses
    kgkpiKeyPairIds,
    kgkpiKeyGroupId,
  )
where

import Network.AWS.CloudFront.Types.KeyPairIds
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A list of identifiers for the public keys that CloudFront can use to verify the signatures of signed URLs and signed cookies.
--
-- /See:/ 'mkKGKeyPairIds' smart constructor.
data KGKeyPairIds = KGKeyPairIds'
  { keyPairIds ::
      Lude.Maybe KeyPairIds,
    keyGroupId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'KGKeyPairIds' with the minimum fields required to make a request.
--
-- * 'keyGroupId' - The identifier of the key group that contains the public keys.
-- * 'keyPairIds' - Undocumented field.
mkKGKeyPairIds ::
  KGKeyPairIds
mkKGKeyPairIds =
  KGKeyPairIds'
    { keyPairIds = Lude.Nothing,
      keyGroupId = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'keyPairIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kgkpiKeyPairIds :: Lens.Lens' KGKeyPairIds (Lude.Maybe KeyPairIds)
kgkpiKeyPairIds = Lens.lens (keyPairIds :: KGKeyPairIds -> Lude.Maybe KeyPairIds) (\s a -> s {keyPairIds = a} :: KGKeyPairIds)
{-# DEPRECATED kgkpiKeyPairIds "Use generic-lens or generic-optics with 'keyPairIds' instead." #-}

-- | The identifier of the key group that contains the public keys.
--
-- /Note:/ Consider using 'keyGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kgkpiKeyGroupId :: Lens.Lens' KGKeyPairIds (Lude.Maybe Lude.Text)
kgkpiKeyGroupId = Lens.lens (keyGroupId :: KGKeyPairIds -> Lude.Maybe Lude.Text) (\s a -> s {keyGroupId = a} :: KGKeyPairIds)
{-# DEPRECATED kgkpiKeyGroupId "Use generic-lens or generic-optics with 'keyGroupId' instead." #-}

instance Lude.FromXML KGKeyPairIds where
  parseXML x =
    KGKeyPairIds'
      Lude.<$> (x Lude..@? "KeyPairIds") Lude.<*> (x Lude..@? "KeyGroupId")
