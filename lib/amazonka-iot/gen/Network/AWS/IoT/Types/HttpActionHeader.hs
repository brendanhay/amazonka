{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.HttpActionHeader
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.HttpActionHeader
  ( HttpActionHeader (..)
  -- * Smart constructor
  , mkHttpActionHeader
  -- * Lenses
  , hahKey
  , hahValue
  ) where

import qualified Network.AWS.IoT.Types.HeaderKey as Types
import qualified Network.AWS.IoT.Types.HeaderValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The HTTP action header.
--
-- /See:/ 'mkHttpActionHeader' smart constructor.
data HttpActionHeader = HttpActionHeader'
  { key :: Types.HeaderKey
    -- ^ The HTTP header key.
  , value :: Types.HeaderValue
    -- ^ The HTTP header value. Substitution templates are supported.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HttpActionHeader' value with any optional fields omitted.
mkHttpActionHeader
    :: Types.HeaderKey -- ^ 'key'
    -> Types.HeaderValue -- ^ 'value'
    -> HttpActionHeader
mkHttpActionHeader key value = HttpActionHeader'{key, value}

-- | The HTTP header key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hahKey :: Lens.Lens' HttpActionHeader Types.HeaderKey
hahKey = Lens.field @"key"
{-# INLINEABLE hahKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | The HTTP header value. Substitution templates are supported.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hahValue :: Lens.Lens' HttpActionHeader Types.HeaderValue
hahValue = Lens.field @"value"
{-# INLINEABLE hahValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromJSON HttpActionHeader where
        toJSON HttpActionHeader{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("key" Core..= key), Core.Just ("value" Core..= value)])

instance Core.FromJSON HttpActionHeader where
        parseJSON
          = Core.withObject "HttpActionHeader" Core.$
              \ x ->
                HttpActionHeader' Core.<$>
                  (x Core..: "key") Core.<*> x Core..: "value"
