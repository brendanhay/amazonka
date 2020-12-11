-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.MssEncryption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.MssEncryption
  ( MssEncryption (..),

    -- * Smart constructor
    mkMssEncryption,

    -- * Lenses
    meSpekeKeyProvider,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types.SpekeKeyProvider
import qualified Network.AWS.Prelude as Lude

-- | A Microsoft Smooth Streaming (MSS) encryption configuration.
--
-- /See:/ 'mkMssEncryption' smart constructor.
newtype MssEncryption = MssEncryption'
  { spekeKeyProvider ::
      SpekeKeyProvider
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MssEncryption' with the minimum fields required to make a request.
--
-- * 'spekeKeyProvider' - Undocumented field.
mkMssEncryption ::
  -- | 'spekeKeyProvider'
  SpekeKeyProvider ->
  MssEncryption
mkMssEncryption pSpekeKeyProvider_ =
  MssEncryption' {spekeKeyProvider = pSpekeKeyProvider_}

-- | Undocumented field.
--
-- /Note:/ Consider using 'spekeKeyProvider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
meSpekeKeyProvider :: Lens.Lens' MssEncryption SpekeKeyProvider
meSpekeKeyProvider = Lens.lens (spekeKeyProvider :: MssEncryption -> SpekeKeyProvider) (\s a -> s {spekeKeyProvider = a} :: MssEncryption)
{-# DEPRECATED meSpekeKeyProvider "Use generic-lens or generic-optics with 'spekeKeyProvider' instead." #-}

instance Lude.FromJSON MssEncryption where
  parseJSON =
    Lude.withObject
      "MssEncryption"
      (\x -> MssEncryption' Lude.<$> (x Lude..: "spekeKeyProvider"))

instance Lude.ToJSON MssEncryption where
  toJSON MssEncryption' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("spekeKeyProvider" Lude..= spekeKeyProvider)]
      )
