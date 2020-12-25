{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.UiTemplateInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.UiTemplateInfo
  ( UiTemplateInfo (..),

    -- * Smart constructor
    mkUiTemplateInfo,

    -- * Lenses
    utiContentSha256,
    utiUrl,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.TemplateContentSha256 as Types
import qualified Network.AWS.SageMaker.Types.TemplateUrl as Types

-- | Container for user interface template information.
--
-- /See:/ 'mkUiTemplateInfo' smart constructor.
data UiTemplateInfo = UiTemplateInfo'
  { -- | The SHA-256 digest of the contents of the template.
    contentSha256 :: Core.Maybe Types.TemplateContentSha256,
    -- | The URL for the user interface template.
    url :: Core.Maybe Types.TemplateUrl
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UiTemplateInfo' value with any optional fields omitted.
mkUiTemplateInfo ::
  UiTemplateInfo
mkUiTemplateInfo =
  UiTemplateInfo' {contentSha256 = Core.Nothing, url = Core.Nothing}

-- | The SHA-256 digest of the contents of the template.
--
-- /Note:/ Consider using 'contentSha256' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utiContentSha256 :: Lens.Lens' UiTemplateInfo (Core.Maybe Types.TemplateContentSha256)
utiContentSha256 = Lens.field @"contentSha256"
{-# DEPRECATED utiContentSha256 "Use generic-lens or generic-optics with 'contentSha256' instead." #-}

-- | The URL for the user interface template.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utiUrl :: Lens.Lens' UiTemplateInfo (Core.Maybe Types.TemplateUrl)
utiUrl = Lens.field @"url"
{-# DEPRECATED utiUrl "Use generic-lens or generic-optics with 'url' instead." #-}

instance Core.FromJSON UiTemplateInfo where
  parseJSON =
    Core.withObject "UiTemplateInfo" Core.$
      \x ->
        UiTemplateInfo'
          Core.<$> (x Core..:? "ContentSha256") Core.<*> (x Core..:? "Url")
