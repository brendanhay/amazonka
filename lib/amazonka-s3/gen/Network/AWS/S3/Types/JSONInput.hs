{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.JSONInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.JSONInput
  ( JSONInput (..)
  -- * Smart constructor
  , mkJSONInput
  -- * Lenses
  , jsoniType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.JSONType as Types

-- | Specifies JSON as object's input serialization format.
--
-- /See:/ 'mkJSONInput' smart constructor.
newtype JSONInput = JSONInput'
  { type' :: Core.Maybe Types.JSONType
    -- ^ The type of JSON. Valid values: Document, Lines.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'JSONInput' value with any optional fields omitted.
mkJSONInput
    :: JSONInput
mkJSONInput = JSONInput'{type' = Core.Nothing}

-- | The type of JSON. Valid values: Document, Lines.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsoniType :: Lens.Lens' JSONInput (Core.Maybe Types.JSONType)
jsoniType = Lens.field @"type'"
{-# INLINEABLE jsoniType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.ToXML JSONInput where
        toXML JSONInput{..}
          = Core.maybe Core.mempty (Core.toXMLElement "Type") type'
