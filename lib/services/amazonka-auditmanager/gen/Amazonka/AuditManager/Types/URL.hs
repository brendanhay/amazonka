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
-- Module      : Amazonka.AuditManager.Types.URL
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.URL where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Short for uniform resource locator. A URL is used as a unique identifier
-- to locate a resource on the internet.
--
-- /See:/ 'newURL' smart constructor.
data URL = URL'
  { -- | The name or word that\'s used as a hyperlink to the URL.
    hyperlinkName :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the internet resource.
    link :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'URL' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hyperlinkName', 'url_hyperlinkName' - The name or word that\'s used as a hyperlink to the URL.
--
-- 'link', 'url_link' - The unique identifier for the internet resource.
newURL ::
  URL
newURL =
  URL'
    { hyperlinkName = Prelude.Nothing,
      link = Prelude.Nothing
    }

-- | The name or word that\'s used as a hyperlink to the URL.
url_hyperlinkName :: Lens.Lens' URL (Prelude.Maybe Prelude.Text)
url_hyperlinkName = Lens.lens (\URL' {hyperlinkName} -> hyperlinkName) (\s@URL' {} a -> s {hyperlinkName = a} :: URL)

-- | The unique identifier for the internet resource.
url_link :: Lens.Lens' URL (Prelude.Maybe Prelude.Text)
url_link = Lens.lens (\URL' {link} -> link) (\s@URL' {} a -> s {link = a} :: URL)

instance Data.FromJSON URL where
  parseJSON =
    Data.withObject
      "URL"
      ( \x ->
          URL'
            Prelude.<$> (x Data..:? "hyperlinkName")
            Prelude.<*> (x Data..:? "link")
      )

instance Prelude.Hashable URL where
  hashWithSalt _salt URL' {..} =
    _salt
      `Prelude.hashWithSalt` hyperlinkName
      `Prelude.hashWithSalt` link

instance Prelude.NFData URL where
  rnf URL' {..} =
    Prelude.rnf hyperlinkName
      `Prelude.seq` Prelude.rnf link
