{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.Template
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.Template
  ( Template (..)
  -- * Smart constructor
  , mkTemplate
  -- * Lenses
  , tName
  , tVersion
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the name and version of the message template to use for the message.
--
-- /See:/ 'mkTemplate' smart constructor.
data Template = Template'
  { name :: Core.Maybe Core.Text
    -- ^ The name of the message template to use for the message. If specified, this value must match the name of an existing message template.
  , version :: Core.Maybe Core.Text
    -- ^ The unique identifier for the version of the message template to use for the message. If specified, this value must match the identifier for an existing template version. To retrieve a list of versions and version identifiers for a template, use the <link>Template Versions resource.
--
-- If you don't specify a value for this property, Amazon Pinpoint uses the /active version/ of the template. The /active version/ is typically the version of a template that's been most recently reviewed and approved for use, depending on your workflow. It isn't necessarily the latest version of a template.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Template' value with any optional fields omitted.
mkTemplate
    :: Template
mkTemplate = Template'{name = Core.Nothing, version = Core.Nothing}

-- | The name of the message template to use for the message. If specified, this value must match the name of an existing message template.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tName :: Lens.Lens' Template (Core.Maybe Core.Text)
tName = Lens.field @"name"
{-# INLINEABLE tName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The unique identifier for the version of the message template to use for the message. If specified, this value must match the identifier for an existing template version. To retrieve a list of versions and version identifiers for a template, use the <link>Template Versions resource.
--
-- If you don't specify a value for this property, Amazon Pinpoint uses the /active version/ of the template. The /active version/ is typically the version of a template that's been most recently reviewed and approved for use, depending on your workflow. It isn't necessarily the latest version of a template.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tVersion :: Lens.Lens' Template (Core.Maybe Core.Text)
tVersion = Lens.field @"version"
{-# INLINEABLE tVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

instance Core.FromJSON Template where
        toJSON Template{..}
          = Core.object
              (Core.catMaybes
                 [("Name" Core..=) Core.<$> name,
                  ("Version" Core..=) Core.<$> version])

instance Core.FromJSON Template where
        parseJSON
          = Core.withObject "Template" Core.$
              \ x ->
                Template' Core.<$>
                  (x Core..:? "Name") Core.<*> x Core..:? "Version"
