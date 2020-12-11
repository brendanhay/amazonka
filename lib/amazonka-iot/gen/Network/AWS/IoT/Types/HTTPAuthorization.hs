-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.HTTPAuthorization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.HTTPAuthorization
  ( HTTPAuthorization (..),

    -- * Smart constructor
    mkHTTPAuthorization,

    -- * Lenses
    httpaSigv4,
  )
where

import Network.AWS.IoT.Types.SigV4Authorization
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The authorization method used to send messages.
--
-- /See:/ 'mkHTTPAuthorization' smart constructor.
newtype HTTPAuthorization = HTTPAuthorization'
  { sigv4 ::
      Lude.Maybe SigV4Authorization
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HTTPAuthorization' with the minimum fields required to make a request.
--
-- * 'sigv4' - Use Sig V4 authorization. For more information, see <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process> .
mkHTTPAuthorization ::
  HTTPAuthorization
mkHTTPAuthorization = HTTPAuthorization' {sigv4 = Lude.Nothing}

-- | Use Sig V4 authorization. For more information, see <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process> .
--
-- /Note:/ Consider using 'sigv4' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpaSigv4 :: Lens.Lens' HTTPAuthorization (Lude.Maybe SigV4Authorization)
httpaSigv4 = Lens.lens (sigv4 :: HTTPAuthorization -> Lude.Maybe SigV4Authorization) (\s a -> s {sigv4 = a} :: HTTPAuthorization)
{-# DEPRECATED httpaSigv4 "Use generic-lens or generic-optics with 'sigv4' instead." #-}

instance Lude.FromJSON HTTPAuthorization where
  parseJSON =
    Lude.withObject
      "HTTPAuthorization"
      (\x -> HTTPAuthorization' Lude.<$> (x Lude..:? "sigv4"))

instance Lude.ToJSON HTTPAuthorization where
  toJSON HTTPAuthorization' {..} =
    Lude.object (Lude.catMaybes [("sigv4" Lude..=) Lude.<$> sigv4])
