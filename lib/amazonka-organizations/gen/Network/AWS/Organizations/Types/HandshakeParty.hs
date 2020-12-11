-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.HandshakeParty
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.HandshakeParty
  ( HandshakeParty (..),

    -- * Smart constructor
    mkHandshakeParty,

    -- * Lenses
    hpId,
    hpType,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types.HandshakePartyType
import qualified Network.AWS.Prelude as Lude

-- | Identifies a participant in a handshake.
--
-- /See:/ 'mkHandshakeParty' smart constructor.
data HandshakeParty = HandshakeParty'
  { id ::
      Lude.Sensitive Lude.Text,
    type' :: HandshakePartyType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HandshakeParty' with the minimum fields required to make a request.
--
-- * 'id' - The unique identifier (ID) for the party.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID string requires "h-" followed by from 8 to 32 lowercase letters or digits.
-- * 'type'' - The type of party.
mkHandshakeParty ::
  -- | 'id'
  Lude.Sensitive Lude.Text ->
  -- | 'type''
  HandshakePartyType ->
  HandshakeParty
mkHandshakeParty pId_ pType_ =
  HandshakeParty' {id = pId_, type' = pType_}

-- | The unique identifier (ID) for the party.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID string requires "h-" followed by from 8 to 32 lowercase letters or digits.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpId :: Lens.Lens' HandshakeParty (Lude.Sensitive Lude.Text)
hpId = Lens.lens (id :: HandshakeParty -> Lude.Sensitive Lude.Text) (\s a -> s {id = a} :: HandshakeParty)
{-# DEPRECATED hpId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The type of party.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpType :: Lens.Lens' HandshakeParty HandshakePartyType
hpType = Lens.lens (type' :: HandshakeParty -> HandshakePartyType) (\s a -> s {type' = a} :: HandshakeParty)
{-# DEPRECATED hpType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON HandshakeParty where
  parseJSON =
    Lude.withObject
      "HandshakeParty"
      ( \x ->
          HandshakeParty'
            Lude.<$> (x Lude..: "Id") Lude.<*> (x Lude..: "Type")
      )

instance Lude.ToJSON HandshakeParty where
  toJSON HandshakeParty' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("Id" Lude..= id), Lude.Just ("Type" Lude..= type')]
      )
