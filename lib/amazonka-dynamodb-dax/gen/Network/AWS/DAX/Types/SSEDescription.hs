{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.Types.SSEDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DAX.Types.SSEDescription
  ( SSEDescription (..),

    -- * Smart constructor
    mkSSEDescription,

    -- * Lenses
    ssedStatus,
  )
where

import Network.AWS.DAX.Types.SSEStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The description of the server-side encryption status on the specified DAX cluster.
--
-- /See:/ 'mkSSEDescription' smart constructor.
newtype SSEDescription = SSEDescription'
  { -- | The current state of server-side encryption:
    --
    --
    --     * @ENABLING@ - Server-side encryption is being enabled.
    --
    --
    --     * @ENABLED@ - Server-side encryption is enabled.
    --
    --
    --     * @DISABLING@ - Server-side encryption is being disabled.
    --
    --
    --     * @DISABLED@ - Server-side encryption is disabled.
    status :: Lude.Maybe SSEStatus
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SSEDescription' with the minimum fields required to make a request.
--
-- * 'status' - The current state of server-side encryption:
--
--
--     * @ENABLING@ - Server-side encryption is being enabled.
--
--
--     * @ENABLED@ - Server-side encryption is enabled.
--
--
--     * @DISABLING@ - Server-side encryption is being disabled.
--
--
--     * @DISABLED@ - Server-side encryption is disabled.
mkSSEDescription ::
  SSEDescription
mkSSEDescription = SSEDescription' {status = Lude.Nothing}

-- | The current state of server-side encryption:
--
--
--     * @ENABLING@ - Server-side encryption is being enabled.
--
--
--     * @ENABLED@ - Server-side encryption is enabled.
--
--
--     * @DISABLING@ - Server-side encryption is being disabled.
--
--
--     * @DISABLED@ - Server-side encryption is disabled.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssedStatus :: Lens.Lens' SSEDescription (Lude.Maybe SSEStatus)
ssedStatus = Lens.lens (status :: SSEDescription -> Lude.Maybe SSEStatus) (\s a -> s {status = a} :: SSEDescription)
{-# DEPRECATED ssedStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Lude.FromJSON SSEDescription where
  parseJSON =
    Lude.withObject
      "SSEDescription"
      (\x -> SSEDescription' Lude.<$> (x Lude..:? "Status"))
