{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ErrorDocument
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ErrorDocument
  ( ErrorDocument (..),

    -- * Smart constructor
    mkErrorDocument,

    -- * Lenses
    edKey,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.Key as Types

-- | The error information.
--
-- /See:/ 'mkErrorDocument' smart constructor.
newtype ErrorDocument = ErrorDocument'
  { -- | The object key name to use when a 4XX class error occurs.
    key :: Types.Key
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ErrorDocument' value with any optional fields omitted.
mkErrorDocument ::
  -- | 'key'
  Types.Key ->
  ErrorDocument
mkErrorDocument key = ErrorDocument' {key}

-- | The object key name to use when a 4XX class error occurs.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edKey :: Lens.Lens' ErrorDocument Types.Key
edKey = Lens.field @"key"
{-# DEPRECATED edKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Core.ToXML ErrorDocument where
  toXML ErrorDocument {..} = Core.toXMLNode "Key" key

instance Core.FromXML ErrorDocument where
  parseXML x = ErrorDocument' Core.<$> (x Core..@ "Key")
