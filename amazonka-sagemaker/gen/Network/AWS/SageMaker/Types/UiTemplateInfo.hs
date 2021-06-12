{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.UiTemplateInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.UiTemplateInfo where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Container for user interface template information.
--
-- /See:/ 'newUiTemplateInfo' smart constructor.
data UiTemplateInfo = UiTemplateInfo'
  { -- | The SHA-256 digest of the contents of the template.
    contentSha256 :: Core.Maybe Core.Text,
    -- | The URL for the user interface template.
    url :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UiTemplateInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentSha256', 'uiTemplateInfo_contentSha256' - The SHA-256 digest of the contents of the template.
--
-- 'url', 'uiTemplateInfo_url' - The URL for the user interface template.
newUiTemplateInfo ::
  UiTemplateInfo
newUiTemplateInfo =
  UiTemplateInfo'
    { contentSha256 = Core.Nothing,
      url = Core.Nothing
    }

-- | The SHA-256 digest of the contents of the template.
uiTemplateInfo_contentSha256 :: Lens.Lens' UiTemplateInfo (Core.Maybe Core.Text)
uiTemplateInfo_contentSha256 = Lens.lens (\UiTemplateInfo' {contentSha256} -> contentSha256) (\s@UiTemplateInfo' {} a -> s {contentSha256 = a} :: UiTemplateInfo)

-- | The URL for the user interface template.
uiTemplateInfo_url :: Lens.Lens' UiTemplateInfo (Core.Maybe Core.Text)
uiTemplateInfo_url = Lens.lens (\UiTemplateInfo' {url} -> url) (\s@UiTemplateInfo' {} a -> s {url = a} :: UiTemplateInfo)

instance Core.FromJSON UiTemplateInfo where
  parseJSON =
    Core.withObject
      "UiTemplateInfo"
      ( \x ->
          UiTemplateInfo'
            Core.<$> (x Core..:? "ContentSha256")
            Core.<*> (x Core..:? "Url")
      )

instance Core.Hashable UiTemplateInfo

instance Core.NFData UiTemplateInfo
