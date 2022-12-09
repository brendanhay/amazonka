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
-- Module      : Amazonka.GuardDuty.Types.AccessKeyDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.AccessKeyDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the access keys.
--
-- /See:/ 'newAccessKeyDetails' smart constructor.
data AccessKeyDetails = AccessKeyDetails'
  { -- | The access key ID of the user.
    accessKeyId :: Prelude.Maybe Prelude.Text,
    -- | The principal ID of the user.
    principalId :: Prelude.Maybe Prelude.Text,
    -- | The name of the user.
    userName :: Prelude.Maybe Prelude.Text,
    -- | The type of the user.
    userType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccessKeyDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessKeyId', 'accessKeyDetails_accessKeyId' - The access key ID of the user.
--
-- 'principalId', 'accessKeyDetails_principalId' - The principal ID of the user.
--
-- 'userName', 'accessKeyDetails_userName' - The name of the user.
--
-- 'userType', 'accessKeyDetails_userType' - The type of the user.
newAccessKeyDetails ::
  AccessKeyDetails
newAccessKeyDetails =
  AccessKeyDetails'
    { accessKeyId = Prelude.Nothing,
      principalId = Prelude.Nothing,
      userName = Prelude.Nothing,
      userType = Prelude.Nothing
    }

-- | The access key ID of the user.
accessKeyDetails_accessKeyId :: Lens.Lens' AccessKeyDetails (Prelude.Maybe Prelude.Text)
accessKeyDetails_accessKeyId = Lens.lens (\AccessKeyDetails' {accessKeyId} -> accessKeyId) (\s@AccessKeyDetails' {} a -> s {accessKeyId = a} :: AccessKeyDetails)

-- | The principal ID of the user.
accessKeyDetails_principalId :: Lens.Lens' AccessKeyDetails (Prelude.Maybe Prelude.Text)
accessKeyDetails_principalId = Lens.lens (\AccessKeyDetails' {principalId} -> principalId) (\s@AccessKeyDetails' {} a -> s {principalId = a} :: AccessKeyDetails)

-- | The name of the user.
accessKeyDetails_userName :: Lens.Lens' AccessKeyDetails (Prelude.Maybe Prelude.Text)
accessKeyDetails_userName = Lens.lens (\AccessKeyDetails' {userName} -> userName) (\s@AccessKeyDetails' {} a -> s {userName = a} :: AccessKeyDetails)

-- | The type of the user.
accessKeyDetails_userType :: Lens.Lens' AccessKeyDetails (Prelude.Maybe Prelude.Text)
accessKeyDetails_userType = Lens.lens (\AccessKeyDetails' {userType} -> userType) (\s@AccessKeyDetails' {} a -> s {userType = a} :: AccessKeyDetails)

instance Data.FromJSON AccessKeyDetails where
  parseJSON =
    Data.withObject
      "AccessKeyDetails"
      ( \x ->
          AccessKeyDetails'
            Prelude.<$> (x Data..:? "accessKeyId")
            Prelude.<*> (x Data..:? "principalId")
            Prelude.<*> (x Data..:? "userName")
            Prelude.<*> (x Data..:? "userType")
      )

instance Prelude.Hashable AccessKeyDetails where
  hashWithSalt _salt AccessKeyDetails' {..} =
    _salt `Prelude.hashWithSalt` accessKeyId
      `Prelude.hashWithSalt` principalId
      `Prelude.hashWithSalt` userName
      `Prelude.hashWithSalt` userType

instance Prelude.NFData AccessKeyDetails where
  rnf AccessKeyDetails' {..} =
    Prelude.rnf accessKeyId
      `Prelude.seq` Prelude.rnf principalId
      `Prelude.seq` Prelude.rnf userName
      `Prelude.seq` Prelude.rnf userType
