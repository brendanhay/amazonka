{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.Grantee
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glacier.Types.Grantee
  ( Grantee (..)
  -- * Smart constructor
  , mkGrantee
  -- * Lenses
  , gType
  , gDisplayName
  , gEmailAddress
  , gID
  , gURI
  ) where

import qualified Network.AWS.Glacier.Types.Type as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the grantee.
--
-- /See:/ 'mkGrantee' smart constructor.
data Grantee = Grantee'
  { type' :: Types.Type
    -- ^ Type of grantee
  , displayName :: Core.Maybe Core.Text
    -- ^ Screen name of the grantee.
  , emailAddress :: Core.Maybe Core.Text
    -- ^ Email address of the grantee.
  , id :: Core.Maybe Core.Text
    -- ^ The canonical user ID of the grantee.
  , uri :: Core.Maybe Core.Text
    -- ^ URI of the grantee group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Grantee' value with any optional fields omitted.
mkGrantee
    :: Types.Type -- ^ 'type\''
    -> Grantee
mkGrantee type'
  = Grantee'{type', displayName = Core.Nothing,
             emailAddress = Core.Nothing, id = Core.Nothing, uri = Core.Nothing}

-- | Type of grantee
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gType :: Lens.Lens' Grantee Types.Type
gType = Lens.field @"type'"
{-# INLINEABLE gType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | Screen name of the grantee.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gDisplayName :: Lens.Lens' Grantee (Core.Maybe Core.Text)
gDisplayName = Lens.field @"displayName"
{-# INLINEABLE gDisplayName #-}
{-# DEPRECATED displayName "Use generic-lens or generic-optics with 'displayName' instead"  #-}

-- | Email address of the grantee.
--
-- /Note:/ Consider using 'emailAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gEmailAddress :: Lens.Lens' Grantee (Core.Maybe Core.Text)
gEmailAddress = Lens.field @"emailAddress"
{-# INLINEABLE gEmailAddress #-}
{-# DEPRECATED emailAddress "Use generic-lens or generic-optics with 'emailAddress' instead"  #-}

-- | The canonical user ID of the grantee.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gID :: Lens.Lens' Grantee (Core.Maybe Core.Text)
gID = Lens.field @"id"
{-# INLINEABLE gID #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | URI of the grantee group.
--
-- /Note:/ Consider using 'uri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gURI :: Lens.Lens' Grantee (Core.Maybe Core.Text)
gURI = Lens.field @"uri"
{-# INLINEABLE gURI #-}
{-# DEPRECATED uri "Use generic-lens or generic-optics with 'uri' instead"  #-}

instance Core.FromJSON Grantee where
        toJSON Grantee{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Type" Core..= type'),
                  ("DisplayName" Core..=) Core.<$> displayName,
                  ("EmailAddress" Core..=) Core.<$> emailAddress,
                  ("ID" Core..=) Core.<$> id, ("URI" Core..=) Core.<$> uri])

instance Core.FromJSON Grantee where
        parseJSON
          = Core.withObject "Grantee" Core.$
              \ x ->
                Grantee' Core.<$>
                  (x Core..: "Type") Core.<*> x Core..:? "DisplayName" Core.<*>
                    x Core..:? "EmailAddress"
                    Core.<*> x Core..:? "ID"
                    Core.<*> x Core..:? "URI"
