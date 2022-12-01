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
-- Module      : Amazonka.MediaLive.Types.HlsS3Settings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.HlsS3Settings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaLive.Types.S3CannedAcl
import qualified Amazonka.Prelude as Prelude

-- | Hls S3 Settings
--
-- /See:/ 'newHlsS3Settings' smart constructor.
data HlsS3Settings = HlsS3Settings'
  { -- | Specify the canned ACL to apply to each S3 request. Defaults to none.
    cannedAcl :: Prelude.Maybe S3CannedAcl
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HlsS3Settings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cannedAcl', 'hlsS3Settings_cannedAcl' - Specify the canned ACL to apply to each S3 request. Defaults to none.
newHlsS3Settings ::
  HlsS3Settings
newHlsS3Settings =
  HlsS3Settings' {cannedAcl = Prelude.Nothing}

-- | Specify the canned ACL to apply to each S3 request. Defaults to none.
hlsS3Settings_cannedAcl :: Lens.Lens' HlsS3Settings (Prelude.Maybe S3CannedAcl)
hlsS3Settings_cannedAcl = Lens.lens (\HlsS3Settings' {cannedAcl} -> cannedAcl) (\s@HlsS3Settings' {} a -> s {cannedAcl = a} :: HlsS3Settings)

instance Core.FromJSON HlsS3Settings where
  parseJSON =
    Core.withObject
      "HlsS3Settings"
      ( \x ->
          HlsS3Settings' Prelude.<$> (x Core..:? "cannedAcl")
      )

instance Prelude.Hashable HlsS3Settings where
  hashWithSalt _salt HlsS3Settings' {..} =
    _salt `Prelude.hashWithSalt` cannedAcl

instance Prelude.NFData HlsS3Settings where
  rnf HlsS3Settings' {..} = Prelude.rnf cannedAcl

instance Core.ToJSON HlsS3Settings where
  toJSON HlsS3Settings' {..} =
    Core.object
      ( Prelude.catMaybes
          [("cannedAcl" Core..=) Prelude.<$> cannedAcl]
      )
