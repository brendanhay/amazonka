{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Grantee
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.Grantee
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.DisplayName as Types
import qualified Network.AWS.S3.Types.EmailAddress as Types
import qualified Network.AWS.S3.Types.ID as Types
import qualified Network.AWS.S3.Types.Type as Types
import qualified Network.AWS.S3.Types.URI as Types

-- | Container for the person being granted permissions.
--
-- /See:/ 'mkGrantee' smart constructor.
data Grantee = Grantee'
  { type' :: Types.Type
    -- ^ Type of grantee
  , displayName :: Core.Maybe Types.DisplayName
    -- ^ Screen name of the grantee.
  , emailAddress :: Core.Maybe Types.EmailAddress
    -- ^ Email address of the grantee.
  , id :: Core.Maybe Types.ID
    -- ^ The canonical user ID of the grantee.
  , uri :: Core.Maybe Types.URI
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
gDisplayName :: Lens.Lens' Grantee (Core.Maybe Types.DisplayName)
gDisplayName = Lens.field @"displayName"
{-# INLINEABLE gDisplayName #-}
{-# DEPRECATED displayName "Use generic-lens or generic-optics with 'displayName' instead"  #-}

-- | Email address of the grantee.
--
-- /Note:/ Consider using 'emailAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gEmailAddress :: Lens.Lens' Grantee (Core.Maybe Types.EmailAddress)
gEmailAddress = Lens.field @"emailAddress"
{-# INLINEABLE gEmailAddress #-}
{-# DEPRECATED emailAddress "Use generic-lens or generic-optics with 'emailAddress' instead"  #-}

-- | The canonical user ID of the grantee.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gID :: Lens.Lens' Grantee (Core.Maybe Types.ID)
gID = Lens.field @"id"
{-# INLINEABLE gID #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | URI of the grantee group.
--
-- /Note:/ Consider using 'uri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gURI :: Lens.Lens' Grantee (Core.Maybe Types.URI)
gURI = Lens.field @"uri"
{-# INLINEABLE gURI #-}
{-# DEPRECATED uri "Use generic-lens or generic-optics with 'uri' instead"  #-}

instance Core.ToXML Grantee where
        toXML Grantee{..}
          = Core.toXMLAttribute "xsi:type" type' Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "DisplayName")
                displayName
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "EmailAddress")
                emailAddress
              Core.<> Core.maybe Core.mempty (Core.toXMLElement "ID") id
              Core.<> Core.maybe Core.mempty (Core.toXMLElement "URI") uri

instance Core.FromXML Grantee where
        parseXML x
          = Grantee' Core.<$>
              (x Core..@@ "xsi:type") Core.<*> x Core..@? "DisplayName" Core.<*>
                x Core..@? "EmailAddress"
                Core.<*> x Core..@? "ID"
                Core.<*> x Core..@? "URI"
