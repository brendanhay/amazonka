{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.IndexDocument
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.IndexDocument
  ( IndexDocument (..),

    -- * Smart constructor
    mkIndexDocument,

    -- * Lenses
    idSuffix,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.Suffix as Types

-- | Container for the @Suffix@ element.
--
-- /See:/ 'mkIndexDocument' smart constructor.
newtype IndexDocument = IndexDocument'
  { -- | A suffix that is appended to a request that is for a directory on the website endpoint (for example,if the suffix is index.html and you make a request to samplebucket/images/ the data that is returned will be for the object with the key name images/index.html) The suffix must not be empty and must not include a slash character.
    suffix :: Types.Suffix
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'IndexDocument' value with any optional fields omitted.
mkIndexDocument ::
  -- | 'suffix'
  Types.Suffix ->
  IndexDocument
mkIndexDocument suffix = IndexDocument' {suffix}

-- | A suffix that is appended to a request that is for a directory on the website endpoint (for example,if the suffix is index.html and you make a request to samplebucket/images/ the data that is returned will be for the object with the key name images/index.html) The suffix must not be empty and must not include a slash character.
--
-- /Note:/ Consider using 'suffix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idSuffix :: Lens.Lens' IndexDocument Types.Suffix
idSuffix = Lens.field @"suffix"
{-# DEPRECATED idSuffix "Use generic-lens or generic-optics with 'suffix' instead." #-}

instance Core.ToXML IndexDocument where
  toXML IndexDocument {..} = Core.toXMLNode "Suffix" suffix

instance Core.FromXML IndexDocument where
  parseXML x = IndexDocument' Core.<$> (x Core..@ "Suffix")
