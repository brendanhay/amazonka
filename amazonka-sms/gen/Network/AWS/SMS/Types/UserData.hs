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
-- Module      : Network.AWS.SMS.Types.UserData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.UserData where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SMS.Types.S3Location

-- | A script that runs on first launch of an Amazon EC2 instance. Used for
-- configuring the server during launch.
--
-- /See:/ 'newUserData' smart constructor.
data UserData = UserData'
  { -- | Amazon S3 location of the user-data script.
    s3Location :: Prelude.Maybe S3Location
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON UserData where
  parseJSON =
    Prelude.withObject
      "UserData"
      ( \x ->
          UserData' Prelude.<$> (x Prelude..:? "s3Location")
      )

instance Prelude.Hashable UserData

instance Prelude.NFData UserData

instance Prelude.ToJSON UserData where
  toJSON UserData' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("s3Location" Prelude..=) Prelude.<$> s3Location]
      )
