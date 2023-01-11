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
-- Module      : Amazonka.MediaLive.Types.FrameCaptureS3Settings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.FrameCaptureS3Settings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.S3CannedAcl
import qualified Amazonka.Prelude as Prelude

-- | Frame Capture S3 Settings
--
-- /See:/ 'newFrameCaptureS3Settings' smart constructor.
data FrameCaptureS3Settings = FrameCaptureS3Settings'
  { -- | Specify the canned ACL to apply to each S3 request. Defaults to none.
    cannedAcl :: Prelude.Maybe S3CannedAcl
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FrameCaptureS3Settings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cannedAcl', 'frameCaptureS3Settings_cannedAcl' - Specify the canned ACL to apply to each S3 request. Defaults to none.
newFrameCaptureS3Settings ::
  FrameCaptureS3Settings
newFrameCaptureS3Settings =
  FrameCaptureS3Settings'
    { cannedAcl =
        Prelude.Nothing
    }

-- | Specify the canned ACL to apply to each S3 request. Defaults to none.
frameCaptureS3Settings_cannedAcl :: Lens.Lens' FrameCaptureS3Settings (Prelude.Maybe S3CannedAcl)
frameCaptureS3Settings_cannedAcl = Lens.lens (\FrameCaptureS3Settings' {cannedAcl} -> cannedAcl) (\s@FrameCaptureS3Settings' {} a -> s {cannedAcl = a} :: FrameCaptureS3Settings)

instance Data.FromJSON FrameCaptureS3Settings where
  parseJSON =
    Data.withObject
      "FrameCaptureS3Settings"
      ( \x ->
          FrameCaptureS3Settings'
            Prelude.<$> (x Data..:? "cannedAcl")
      )

instance Prelude.Hashable FrameCaptureS3Settings where
  hashWithSalt _salt FrameCaptureS3Settings' {..} =
    _salt `Prelude.hashWithSalt` cannedAcl

instance Prelude.NFData FrameCaptureS3Settings where
  rnf FrameCaptureS3Settings' {..} =
    Prelude.rnf cannedAcl

instance Data.ToJSON FrameCaptureS3Settings where
  toJSON FrameCaptureS3Settings' {..} =
    Data.object
      ( Prelude.catMaybes
          [("cannedAcl" Data..=) Prelude.<$> cannedAcl]
      )
