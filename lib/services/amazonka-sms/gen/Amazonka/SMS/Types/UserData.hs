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
-- Module      : Amazonka.SMS.Types.UserData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SMS.Types.UserData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SMS.Types.S3Location

-- | A script that runs on first launch of an Amazon EC2 instance. Used for
-- configuring the server during launch.
--
-- /See:/ 'newUserData' smart constructor.
data UserData = UserData'
  { -- | Amazon S3 location of the user-data script.
    s3Location :: Prelude.Maybe S3Location
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Location', 'userData_s3Location' - Amazon S3 location of the user-data script.
newUserData ::
  UserData
newUserData = UserData' {s3Location = Prelude.Nothing}

-- | Amazon S3 location of the user-data script.
userData_s3Location :: Lens.Lens' UserData (Prelude.Maybe S3Location)
userData_s3Location = Lens.lens (\UserData' {s3Location} -> s3Location) (\s@UserData' {} a -> s {s3Location = a} :: UserData)

instance Data.FromJSON UserData where
  parseJSON =
    Data.withObject
      "UserData"
      ( \x ->
          UserData' Prelude.<$> (x Data..:? "s3Location")
      )

instance Prelude.Hashable UserData where
  hashWithSalt _salt UserData' {..} =
    _salt `Prelude.hashWithSalt` s3Location

instance Prelude.NFData UserData where
  rnf UserData' {..} = Prelude.rnf s3Location

instance Data.ToJSON UserData where
  toJSON UserData' {..} =
    Data.object
      ( Prelude.catMaybes
          [("s3Location" Data..=) Prelude.<$> s3Location]
      )
