{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Container for user interface template information.
--
-- /See:/ 'newUiTemplateInfo' smart constructor.
data UiTemplateInfo = UiTemplateInfo'
  { -- | The SHA-256 digest of the contents of the template.
    contentSha256 :: Prelude.Maybe Prelude.Text,
    -- | The URL for the user interface template.
    url :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { contentSha256 = Prelude.Nothing,
      url = Prelude.Nothing
    }

-- | The SHA-256 digest of the contents of the template.
uiTemplateInfo_contentSha256 :: Lens.Lens' UiTemplateInfo (Prelude.Maybe Prelude.Text)
uiTemplateInfo_contentSha256 = Lens.lens (\UiTemplateInfo' {contentSha256} -> contentSha256) (\s@UiTemplateInfo' {} a -> s {contentSha256 = a} :: UiTemplateInfo)

-- | The URL for the user interface template.
uiTemplateInfo_url :: Lens.Lens' UiTemplateInfo (Prelude.Maybe Prelude.Text)
uiTemplateInfo_url = Lens.lens (\UiTemplateInfo' {url} -> url) (\s@UiTemplateInfo' {} a -> s {url = a} :: UiTemplateInfo)

instance Prelude.FromJSON UiTemplateInfo where
  parseJSON =
    Prelude.withObject
      "UiTemplateInfo"
      ( \x ->
          UiTemplateInfo'
            Prelude.<$> (x Prelude..:? "ContentSha256")
            Prelude.<*> (x Prelude..:? "Url")
      )

instance Prelude.Hashable UiTemplateInfo

instance Prelude.NFData UiTemplateInfo
