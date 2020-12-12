{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.TLSContext
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.TLSContext
  ( TLSContext (..),

    -- * Smart constructor
    mkTLSContext,

    -- * Lenses
    tcServerName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the TLS context to use for the test authorizer request.
--
-- /See:/ 'mkTLSContext' smart constructor.
newtype TLSContext = TLSContext'
  { serverName ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TLSContext' with the minimum fields required to make a request.
--
-- * 'serverName' - The value of the @serverName@ key in a TLS authorization request.
mkTLSContext ::
  TLSContext
mkTLSContext = TLSContext' {serverName = Lude.Nothing}

-- | The value of the @serverName@ key in a TLS authorization request.
--
-- /Note:/ Consider using 'serverName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcServerName :: Lens.Lens' TLSContext (Lude.Maybe Lude.Text)
tcServerName = Lens.lens (serverName :: TLSContext -> Lude.Maybe Lude.Text) (\s a -> s {serverName = a} :: TLSContext)
{-# DEPRECATED tcServerName "Use generic-lens or generic-optics with 'serverName' instead." #-}

instance Lude.ToJSON TLSContext where
  toJSON TLSContext' {..} =
    Lude.object
      (Lude.catMaybes [("serverName" Lude..=) Lude.<$> serverName])
