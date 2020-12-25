{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.Grantee
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.Grantee
  ( Grantee (..),

    -- * Smart constructor
    mkGrantee,

    -- * Lenses
    gType,
    gDisplayName,
    gEmailAddress,
    gID,
    gURI,
  )
where

import qualified Network.AWS.Glacier.Types.String as Types
import qualified Network.AWS.Glacier.Types.Type as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the grantee.
--
-- /See:/ 'mkGrantee' smart constructor.
data Grantee = Grantee'
  { -- | Type of grantee
    type' :: Types.Type,
    -- | Screen name of the grantee.
    displayName :: Core.Maybe Types.String,
    -- | Email address of the grantee.
    emailAddress :: Core.Maybe Types.String,
    -- | The canonical user ID of the grantee.
    id :: Core.Maybe Types.String,
    -- | URI of the grantee group.
    uri :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Grantee' value with any optional fields omitted.
mkGrantee ::
  -- | 'type\''
  Types.Type ->
  Grantee
mkGrantee type' =
  Grantee'
    { type',
      displayName = Core.Nothing,
      emailAddress = Core.Nothing,
      id = Core.Nothing,
      uri = Core.Nothing
    }

-- | Type of grantee
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gType :: Lens.Lens' Grantee Types.Type
gType = Lens.field @"type'"
{-# DEPRECATED gType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | Screen name of the grantee.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gDisplayName :: Lens.Lens' Grantee (Core.Maybe Types.String)
gDisplayName = Lens.field @"displayName"
{-# DEPRECATED gDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | Email address of the grantee.
--
-- /Note:/ Consider using 'emailAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gEmailAddress :: Lens.Lens' Grantee (Core.Maybe Types.String)
gEmailAddress = Lens.field @"emailAddress"
{-# DEPRECATED gEmailAddress "Use generic-lens or generic-optics with 'emailAddress' instead." #-}

-- | The canonical user ID of the grantee.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gID :: Lens.Lens' Grantee (Core.Maybe Types.String)
gID = Lens.field @"id"
{-# DEPRECATED gID "Use generic-lens or generic-optics with 'id' instead." #-}

-- | URI of the grantee group.
--
-- /Note:/ Consider using 'uri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gURI :: Lens.Lens' Grantee (Core.Maybe Types.String)
gURI = Lens.field @"uri"
{-# DEPRECATED gURI "Use generic-lens or generic-optics with 'uri' instead." #-}

instance Core.FromJSON Grantee where
  toJSON Grantee {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Type" Core..= type'),
            ("DisplayName" Core..=) Core.<$> displayName,
            ("EmailAddress" Core..=) Core.<$> emailAddress,
            ("ID" Core..=) Core.<$> id,
            ("URI" Core..=) Core.<$> uri
          ]
      )

instance Core.FromJSON Grantee where
  parseJSON =
    Core.withObject "Grantee" Core.$
      \x ->
        Grantee'
          Core.<$> (x Core..: "Type")
          Core.<*> (x Core..:? "DisplayName")
          Core.<*> (x Core..:? "EmailAddress")
          Core.<*> (x Core..:? "ID")
          Core.<*> (x Core..:? "URI")
