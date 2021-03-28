{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.TemplateActiveVersionRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.TemplateActiveVersionRequest
  ( TemplateActiveVersionRequest (..)
  -- * Smart constructor
  , mkTemplateActiveVersionRequest
  -- * Lenses
  , tavrVersion
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies which version of a message template to use as the active version of the template.
--
-- /See:/ 'mkTemplateActiveVersionRequest' smart constructor.
newtype TemplateActiveVersionRequest = TemplateActiveVersionRequest'
  { version :: Core.Maybe Core.Text
    -- ^ The version of the message template to use as the active version of the template. Valid values are: latest, for the most recent version of the template; or, the unique identifier for any existing version of the template. If you specify an identifier, the value must match the identifier for an existing template version. To retrieve a list of versions and version identifiers for a template, use the <link>Template Versions resource.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'TemplateActiveVersionRequest' value with any optional fields omitted.
mkTemplateActiveVersionRequest
    :: TemplateActiveVersionRequest
mkTemplateActiveVersionRequest
  = TemplateActiveVersionRequest'{version = Core.Nothing}

-- | The version of the message template to use as the active version of the template. Valid values are: latest, for the most recent version of the template; or, the unique identifier for any existing version of the template. If you specify an identifier, the value must match the identifier for an existing template version. To retrieve a list of versions and version identifiers for a template, use the <link>Template Versions resource.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tavrVersion :: Lens.Lens' TemplateActiveVersionRequest (Core.Maybe Core.Text)
tavrVersion = Lens.field @"version"
{-# INLINEABLE tavrVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

instance Core.FromJSON TemplateActiveVersionRequest where
        toJSON TemplateActiveVersionRequest{..}
          = Core.object
              (Core.catMaybes [("Version" Core..=) Core.<$> version])
