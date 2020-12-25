{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
import qualified Network.AWS.Organizations.Types.HandshakePartyId as Types
import qualified Network.AWS.Organizations.Types.HandshakePartyType as Types
import qualified Network.AWS.Prelude as Core

-- | Identifies a participant in a handshake.
--
-- /See:/ 'mkHandshakeParty' smart constructor.
data HandshakeParty = HandshakeParty'
  { -- | The unique identifier (ID) for the party.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID string requires "h-" followed by from 8 to 32 lowercase letters or digits.
    id :: Types.HandshakePartyId,
    -- | The type of party.
    type' :: Types.HandshakePartyType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HandshakeParty' value with any optional fields omitted.
mkHandshakeParty ::
  -- | 'id'
  Types.HandshakePartyId ->
  -- | 'type\''
  Types.HandshakePartyType ->
  HandshakeParty
mkHandshakeParty id type' = HandshakeParty' {id, type'}

-- | The unique identifier (ID) for the party.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID string requires "h-" followed by from 8 to 32 lowercase letters or digits.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpId :: Lens.Lens' HandshakeParty Types.HandshakePartyId
hpId = Lens.field @"id"
{-# DEPRECATED hpId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The type of party.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpType :: Lens.Lens' HandshakeParty Types.HandshakePartyType
hpType = Lens.field @"type'"
{-# DEPRECATED hpType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON HandshakeParty where
  toJSON HandshakeParty {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("Id" Core..= id), Core.Just ("Type" Core..= type')]
      )

instance Core.FromJSON HandshakeParty where
  parseJSON =
    Core.withObject "HandshakeParty" Core.$
      \x ->
        HandshakeParty'
          Core.<$> (x Core..: "Id") Core.<*> (x Core..: "Type")
