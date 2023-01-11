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
-- Module      : Amazonka.SageMaker.Types.UiTemplateInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.UiTemplateInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Container for user interface template information.
--
-- /See:/ 'newUiTemplateInfo' smart constructor.
data UiTemplateInfo = UiTemplateInfo'
  { -- | The SHA-256 digest of the contents of the template.
    contentSha256 :: Prelude.Maybe Prelude.Text,
    -- | The URL for the user interface template.
    url :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON UiTemplateInfo where
  parseJSON =
    Data.withObject
      "UiTemplateInfo"
      ( \x ->
          UiTemplateInfo'
            Prelude.<$> (x Data..:? "ContentSha256")
            Prelude.<*> (x Data..:? "Url")
      )

instance Prelude.Hashable UiTemplateInfo where
  hashWithSalt _salt UiTemplateInfo' {..} =
    _salt `Prelude.hashWithSalt` contentSha256
      `Prelude.hashWithSalt` url

instance Prelude.NFData UiTemplateInfo where
  rnf UiTemplateInfo' {..} =
    Prelude.rnf contentSha256
      `Prelude.seq` Prelude.rnf url
