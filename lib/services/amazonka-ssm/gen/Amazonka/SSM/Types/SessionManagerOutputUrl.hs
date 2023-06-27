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
-- Module      : Amazonka.SSM.Types.SessionManagerOutputUrl
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.SessionManagerOutputUrl where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Reserved for future use.
--
-- /See:/ 'newSessionManagerOutputUrl' smart constructor.
data SessionManagerOutputUrl = SessionManagerOutputUrl'
  { -- | Reserved for future use.
    cloudWatchOutputUrl :: Prelude.Maybe Prelude.Text,
    -- | Reserved for future use.
    s3OutputUrl :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SessionManagerOutputUrl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudWatchOutputUrl', 'sessionManagerOutputUrl_cloudWatchOutputUrl' - Reserved for future use.
--
-- 's3OutputUrl', 'sessionManagerOutputUrl_s3OutputUrl' - Reserved for future use.
newSessionManagerOutputUrl ::
  SessionManagerOutputUrl
newSessionManagerOutputUrl =
  SessionManagerOutputUrl'
    { cloudWatchOutputUrl =
        Prelude.Nothing,
      s3OutputUrl = Prelude.Nothing
    }

-- | Reserved for future use.
sessionManagerOutputUrl_cloudWatchOutputUrl :: Lens.Lens' SessionManagerOutputUrl (Prelude.Maybe Prelude.Text)
sessionManagerOutputUrl_cloudWatchOutputUrl = Lens.lens (\SessionManagerOutputUrl' {cloudWatchOutputUrl} -> cloudWatchOutputUrl) (\s@SessionManagerOutputUrl' {} a -> s {cloudWatchOutputUrl = a} :: SessionManagerOutputUrl)

-- | Reserved for future use.
sessionManagerOutputUrl_s3OutputUrl :: Lens.Lens' SessionManagerOutputUrl (Prelude.Maybe Prelude.Text)
sessionManagerOutputUrl_s3OutputUrl = Lens.lens (\SessionManagerOutputUrl' {s3OutputUrl} -> s3OutputUrl) (\s@SessionManagerOutputUrl' {} a -> s {s3OutputUrl = a} :: SessionManagerOutputUrl)

instance Data.FromJSON SessionManagerOutputUrl where
  parseJSON =
    Data.withObject
      "SessionManagerOutputUrl"
      ( \x ->
          SessionManagerOutputUrl'
            Prelude.<$> (x Data..:? "CloudWatchOutputUrl")
            Prelude.<*> (x Data..:? "S3OutputUrl")
      )

instance Prelude.Hashable SessionManagerOutputUrl where
  hashWithSalt _salt SessionManagerOutputUrl' {..} =
    _salt
      `Prelude.hashWithSalt` cloudWatchOutputUrl
      `Prelude.hashWithSalt` s3OutputUrl

instance Prelude.NFData SessionManagerOutputUrl where
  rnf SessionManagerOutputUrl' {..} =
    Prelude.rnf cloudWatchOutputUrl
      `Prelude.seq` Prelude.rnf s3OutputUrl
