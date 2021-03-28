{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.UploadMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkDocs.Types.UploadMetadata
  ( UploadMetadata (..)
  -- * Smart constructor
  , mkUploadMetadata
  -- * Lenses
  , umSignedHeaders
  , umUploadUrl
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkDocs.Types.HeaderNameType as Types
import qualified Network.AWS.WorkDocs.Types.HeaderValueType as Types
import qualified Network.AWS.WorkDocs.Types.UrlType as Types

-- | Describes the upload.
--
-- /See:/ 'mkUploadMetadata' smart constructor.
data UploadMetadata = UploadMetadata'
  { signedHeaders :: Core.Maybe (Core.HashMap Types.HeaderNameType Types.HeaderValueType)
    -- ^ The signed headers.
  , uploadUrl :: Core.Maybe Types.UrlType
    -- ^ The URL of the upload.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UploadMetadata' value with any optional fields omitted.
mkUploadMetadata
    :: UploadMetadata
mkUploadMetadata
  = UploadMetadata'{signedHeaders = Core.Nothing,
                    uploadUrl = Core.Nothing}

-- | The signed headers.
--
-- /Note:/ Consider using 'signedHeaders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umSignedHeaders :: Lens.Lens' UploadMetadata (Core.Maybe (Core.HashMap Types.HeaderNameType Types.HeaderValueType))
umSignedHeaders = Lens.field @"signedHeaders"
{-# INLINEABLE umSignedHeaders #-}
{-# DEPRECATED signedHeaders "Use generic-lens or generic-optics with 'signedHeaders' instead"  #-}

-- | The URL of the upload.
--
-- /Note:/ Consider using 'uploadUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umUploadUrl :: Lens.Lens' UploadMetadata (Core.Maybe Types.UrlType)
umUploadUrl = Lens.field @"uploadUrl"
{-# INLINEABLE umUploadUrl #-}
{-# DEPRECATED uploadUrl "Use generic-lens or generic-optics with 'uploadUrl' instead"  #-}

instance Core.FromJSON UploadMetadata where
        parseJSON
          = Core.withObject "UploadMetadata" Core.$
              \ x ->
                UploadMetadata' Core.<$>
                  (x Core..:? "SignedHeaders") Core.<*> x Core..:? "UploadUrl"
