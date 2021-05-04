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
-- Module      : Network.AWS.SSM.Types.SessionManagerOutputUrl
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.SessionManagerOutputUrl where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Reserved for future use.
--
-- /See:/ 'newSessionManagerOutputUrl' smart constructor.
data SessionManagerOutputUrl = SessionManagerOutputUrl'
  { -- | Reserved for future use.
    s3OutputUrl :: Prelude.Maybe Prelude.Text,
    -- | Reserved for future use.
    cloudWatchOutputUrl :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SessionManagerOutputUrl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3OutputUrl', 'sessionManagerOutputUrl_s3OutputUrl' - Reserved for future use.
--
-- 'cloudWatchOutputUrl', 'sessionManagerOutputUrl_cloudWatchOutputUrl' - Reserved for future use.
newSessionManagerOutputUrl ::
  SessionManagerOutputUrl
newSessionManagerOutputUrl =
  SessionManagerOutputUrl'
    { s3OutputUrl =
        Prelude.Nothing,
      cloudWatchOutputUrl = Prelude.Nothing
    }

-- | Reserved for future use.
sessionManagerOutputUrl_s3OutputUrl :: Lens.Lens' SessionManagerOutputUrl (Prelude.Maybe Prelude.Text)
sessionManagerOutputUrl_s3OutputUrl = Lens.lens (\SessionManagerOutputUrl' {s3OutputUrl} -> s3OutputUrl) (\s@SessionManagerOutputUrl' {} a -> s {s3OutputUrl = a} :: SessionManagerOutputUrl)

-- | Reserved for future use.
sessionManagerOutputUrl_cloudWatchOutputUrl :: Lens.Lens' SessionManagerOutputUrl (Prelude.Maybe Prelude.Text)
sessionManagerOutputUrl_cloudWatchOutputUrl = Lens.lens (\SessionManagerOutputUrl' {cloudWatchOutputUrl} -> cloudWatchOutputUrl) (\s@SessionManagerOutputUrl' {} a -> s {cloudWatchOutputUrl = a} :: SessionManagerOutputUrl)

instance Prelude.FromJSON SessionManagerOutputUrl where
  parseJSON =
    Prelude.withObject
      "SessionManagerOutputUrl"
      ( \x ->
          SessionManagerOutputUrl'
            Prelude.<$> (x Prelude..:? "S3OutputUrl")
            Prelude.<*> (x Prelude..:? "CloudWatchOutputUrl")
      )

instance Prelude.Hashable SessionManagerOutputUrl

instance Prelude.NFData SessionManagerOutputUrl
