{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.CommonPrefix
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.CommonPrefix
  ( CommonPrefix (..)
  -- * Smart constructor
  , mkCommonPrefix
  -- * Lenses
  , cpPrefix
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.Prefix as Types

-- | Container for all (if there are any) keys between Prefix and the next occurrence of the string specified by a delimiter. CommonPrefixes lists keys that act like subdirectories in the directory specified by Prefix. For example, if the prefix is notes/ and the delimiter is a slash (/) as in notes/summer/july, the common prefix is notes/summer/. 
--
-- /See:/ 'mkCommonPrefix' smart constructor.
newtype CommonPrefix = CommonPrefix'
  { prefix :: Core.Maybe Types.Prefix
    -- ^ Container for the specified common prefix.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CommonPrefix' value with any optional fields omitted.
mkCommonPrefix
    :: CommonPrefix
mkCommonPrefix = CommonPrefix'{prefix = Core.Nothing}

-- | Container for the specified common prefix.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpPrefix :: Lens.Lens' CommonPrefix (Core.Maybe Types.Prefix)
cpPrefix = Lens.field @"prefix"
{-# INLINEABLE cpPrefix #-}
{-# DEPRECATED prefix "Use generic-lens or generic-optics with 'prefix' instead"  #-}

instance Core.FromXML CommonPrefix where
        parseXML x = CommonPrefix' Core.<$> (x Core..@? "Prefix")
