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
-- Module      : Amazonka.Kendra.Types.OneDriveUsers
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.OneDriveUsers where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Kendra.Types.S3Path
import qualified Amazonka.Prelude as Prelude

-- | User accounts whose documents should be indexed.
--
-- /See:/ 'newOneDriveUsers' smart constructor.
data OneDriveUsers = OneDriveUsers'
  { -- | A list of users whose documents should be indexed. Specify the user
    -- names in email format, for example, @username\@tenantdomain@. If you
    -- need to index the documents of more than 100 users, use the
    -- @OneDriveUserS3Path@ field to specify the location of a file containing
    -- a list of users.
    oneDriveUserList :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The S3 bucket location of a file containing a list of users whose
    -- documents should be indexed.
    oneDriveUserS3Path :: Prelude.Maybe S3Path
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OneDriveUsers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'oneDriveUserList', 'oneDriveUsers_oneDriveUserList' - A list of users whose documents should be indexed. Specify the user
-- names in email format, for example, @username\@tenantdomain@. If you
-- need to index the documents of more than 100 users, use the
-- @OneDriveUserS3Path@ field to specify the location of a file containing
-- a list of users.
--
-- 'oneDriveUserS3Path', 'oneDriveUsers_oneDriveUserS3Path' - The S3 bucket location of a file containing a list of users whose
-- documents should be indexed.
newOneDriveUsers ::
  OneDriveUsers
newOneDriveUsers =
  OneDriveUsers'
    { oneDriveUserList = Prelude.Nothing,
      oneDriveUserS3Path = Prelude.Nothing
    }

-- | A list of users whose documents should be indexed. Specify the user
-- names in email format, for example, @username\@tenantdomain@. If you
-- need to index the documents of more than 100 users, use the
-- @OneDriveUserS3Path@ field to specify the location of a file containing
-- a list of users.
oneDriveUsers_oneDriveUserList :: Lens.Lens' OneDriveUsers (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
oneDriveUsers_oneDriveUserList = Lens.lens (\OneDriveUsers' {oneDriveUserList} -> oneDriveUserList) (\s@OneDriveUsers' {} a -> s {oneDriveUserList = a} :: OneDriveUsers) Prelude.. Lens.mapping Lens.coerced

-- | The S3 bucket location of a file containing a list of users whose
-- documents should be indexed.
oneDriveUsers_oneDriveUserS3Path :: Lens.Lens' OneDriveUsers (Prelude.Maybe S3Path)
oneDriveUsers_oneDriveUserS3Path = Lens.lens (\OneDriveUsers' {oneDriveUserS3Path} -> oneDriveUserS3Path) (\s@OneDriveUsers' {} a -> s {oneDriveUserS3Path = a} :: OneDriveUsers)

instance Core.FromJSON OneDriveUsers where
  parseJSON =
    Core.withObject
      "OneDriveUsers"
      ( \x ->
          OneDriveUsers'
            Prelude.<$> (x Core..:? "OneDriveUserList")
            Prelude.<*> (x Core..:? "OneDriveUserS3Path")
      )

instance Prelude.Hashable OneDriveUsers where
  hashWithSalt _salt OneDriveUsers' {..} =
    _salt `Prelude.hashWithSalt` oneDriveUserList
      `Prelude.hashWithSalt` oneDriveUserS3Path

instance Prelude.NFData OneDriveUsers where
  rnf OneDriveUsers' {..} =
    Prelude.rnf oneDriveUserList
      `Prelude.seq` Prelude.rnf oneDriveUserS3Path

instance Core.ToJSON OneDriveUsers where
  toJSON OneDriveUsers' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("OneDriveUserList" Core..=)
              Prelude.<$> oneDriveUserList,
            ("OneDriveUserS3Path" Core..=)
              Prelude.<$> oneDriveUserS3Path
          ]
      )
