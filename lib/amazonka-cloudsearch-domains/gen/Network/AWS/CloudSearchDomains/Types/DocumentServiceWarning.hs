{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearchDomains.Types.DocumentServiceWarning
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearchDomains.Types.DocumentServiceWarning
  ( DocumentServiceWarning (..),

    -- * Smart constructor
    mkDocumentServiceWarning,

    -- * Lenses
    dswMessage,
  )
where

import qualified Network.AWS.CloudSearchDomains.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A warning returned by the document service when an issue is discovered while processing an upload request.
--
-- /See:/ 'mkDocumentServiceWarning' smart constructor.
newtype DocumentServiceWarning = DocumentServiceWarning'
  { -- | The description for a warning returned by the document service.
    message :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DocumentServiceWarning' value with any optional fields omitted.
mkDocumentServiceWarning ::
  DocumentServiceWarning
mkDocumentServiceWarning =
  DocumentServiceWarning' {message = Core.Nothing}

-- | The description for a warning returned by the document service.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dswMessage :: Lens.Lens' DocumentServiceWarning (Core.Maybe Types.String)
dswMessage = Lens.field @"message"
{-# DEPRECATED dswMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Core.FromJSON DocumentServiceWarning where
  parseJSON =
    Core.withObject "DocumentServiceWarning" Core.$
      \x -> DocumentServiceWarning' Core.<$> (x Core..:? "message")
