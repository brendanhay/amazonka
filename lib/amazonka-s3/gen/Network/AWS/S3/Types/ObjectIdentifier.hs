{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ObjectIdentifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.ObjectIdentifier
  ( ObjectIdentifier (..)
  -- * Smart constructor
  , mkObjectIdentifier
  -- * Lenses
  , oiKey
  , oiVersionId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.Key as Types

-- | Object Identifier is unique value to identify objects.
--
-- /See:/ 'mkObjectIdentifier' smart constructor.
data ObjectIdentifier = ObjectIdentifier'
  { key :: Types.Key
    -- ^ Key name of the object to delete.
  , versionId :: Core.Maybe Types.ObjectVersionId
    -- ^ VersionId for the specific version of the object to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ObjectIdentifier' value with any optional fields omitted.
mkObjectIdentifier
    :: Types.Key -- ^ 'key'
    -> ObjectIdentifier
mkObjectIdentifier key
  = ObjectIdentifier'{key, versionId = Core.Nothing}

-- | Key name of the object to delete.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oiKey :: Lens.Lens' ObjectIdentifier Types.Key
oiKey = Lens.field @"key"
{-# INLINEABLE oiKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | VersionId for the specific version of the object to delete.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oiVersionId :: Lens.Lens' ObjectIdentifier (Core.Maybe Types.ObjectVersionId)
oiVersionId = Lens.field @"versionId"
{-# INLINEABLE oiVersionId #-}
{-# DEPRECATED versionId "Use generic-lens or generic-optics with 'versionId' instead"  #-}

instance Core.ToXML ObjectIdentifier where
        toXML ObjectIdentifier{..}
          = Core.toXMLElement "Key" key Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "VersionId") versionId
