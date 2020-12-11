-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.ClientProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.ClientProperties
  ( ClientProperties (..),

    -- * Smart constructor
    mkClientProperties,

    -- * Lenses
    cpReconnectEnabled,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WorkSpaces.Types.ReconnectEnum

-- | Describes an Amazon WorkSpaces client.
--
-- /See:/ 'mkClientProperties' smart constructor.
newtype ClientProperties = ClientProperties'
  { reconnectEnabled ::
      Lude.Maybe ReconnectEnum
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ClientProperties' with the minimum fields required to make a request.
--
-- * 'reconnectEnabled' - Specifies whether users can cache their credentials on the Amazon WorkSpaces client. When enabled, users can choose to reconnect to their WorkSpaces without re-entering their credentials.
mkClientProperties ::
  ClientProperties
mkClientProperties =
  ClientProperties' {reconnectEnabled = Lude.Nothing}

-- | Specifies whether users can cache their credentials on the Amazon WorkSpaces client. When enabled, users can choose to reconnect to their WorkSpaces without re-entering their credentials.
--
-- /Note:/ Consider using 'reconnectEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpReconnectEnabled :: Lens.Lens' ClientProperties (Lude.Maybe ReconnectEnum)
cpReconnectEnabled = Lens.lens (reconnectEnabled :: ClientProperties -> Lude.Maybe ReconnectEnum) (\s a -> s {reconnectEnabled = a} :: ClientProperties)
{-# DEPRECATED cpReconnectEnabled "Use generic-lens or generic-optics with 'reconnectEnabled' instead." #-}

instance Lude.FromJSON ClientProperties where
  parseJSON =
    Lude.withObject
      "ClientProperties"
      (\x -> ClientProperties' Lude.<$> (x Lude..:? "ReconnectEnabled"))

instance Lude.ToJSON ClientProperties where
  toJSON ClientProperties' {..} =
    Lude.object
      ( Lude.catMaybes
          [("ReconnectEnabled" Lude..=) Lude.<$> reconnectEnabled]
      )
