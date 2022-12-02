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
-- Module      : Amazonka.MediaLive.Types.ArchiveS3Settings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.ArchiveS3Settings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.S3CannedAcl
import qualified Amazonka.Prelude as Prelude

-- | Archive S3 Settings
--
-- /See:/ 'newArchiveS3Settings' smart constructor.
data ArchiveS3Settings = ArchiveS3Settings'
  { -- | Specify the canned ACL to apply to each S3 request. Defaults to none.
    cannedAcl :: Prelude.Maybe S3CannedAcl
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ArchiveS3Settings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cannedAcl', 'archiveS3Settings_cannedAcl' - Specify the canned ACL to apply to each S3 request. Defaults to none.
newArchiveS3Settings ::
  ArchiveS3Settings
newArchiveS3Settings =
  ArchiveS3Settings' {cannedAcl = Prelude.Nothing}

-- | Specify the canned ACL to apply to each S3 request. Defaults to none.
archiveS3Settings_cannedAcl :: Lens.Lens' ArchiveS3Settings (Prelude.Maybe S3CannedAcl)
archiveS3Settings_cannedAcl = Lens.lens (\ArchiveS3Settings' {cannedAcl} -> cannedAcl) (\s@ArchiveS3Settings' {} a -> s {cannedAcl = a} :: ArchiveS3Settings)

instance Data.FromJSON ArchiveS3Settings where
  parseJSON =
    Data.withObject
      "ArchiveS3Settings"
      ( \x ->
          ArchiveS3Settings'
            Prelude.<$> (x Data..:? "cannedAcl")
      )

instance Prelude.Hashable ArchiveS3Settings where
  hashWithSalt _salt ArchiveS3Settings' {..} =
    _salt `Prelude.hashWithSalt` cannedAcl

instance Prelude.NFData ArchiveS3Settings where
  rnf ArchiveS3Settings' {..} = Prelude.rnf cannedAcl

instance Data.ToJSON ArchiveS3Settings where
  toJSON ArchiveS3Settings' {..} =
    Data.object
      ( Prelude.catMaybes
          [("cannedAcl" Data..=) Prelude.<$> cannedAcl]
      )
