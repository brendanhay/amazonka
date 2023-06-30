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
-- Module      : Amazonka.IAM.Types.AccessKeyInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IAM.Types.AccessKeyInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types.StatusType
import qualified Amazonka.Prelude as Prelude

-- | Contains information about an Amazon Web Services access key.
--
-- This data type is used as a response element in the CreateAccessKey and
-- ListAccessKeys operations.
--
-- The @SecretAccessKey@ value is returned only in response to
-- CreateAccessKey. You can get a secret access key only when you first
-- create an access key; you cannot recover the secret access key later. If
-- you lose a secret access key, you must create a new access key.
--
-- /See:/ 'newAccessKeyInfo' smart constructor.
data AccessKeyInfo = AccessKeyInfo'
  { -- | The date when the access key was created.
    createDate :: Prelude.Maybe Data.ISO8601,
    -- | The name of the IAM user that the access key is associated with.
    userName :: Prelude.Text,
    -- | The ID for this access key.
    accessKeyId :: Core.AccessKey,
    -- | The status of the access key. @Active@ means that the key is valid for
    -- API calls, while @Inactive@ means it is not.
    status :: StatusType,
    -- | The secret key used to sign requests.
    secretAccessKey :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccessKeyInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createDate', 'accessKeyInfo_createDate' - The date when the access key was created.
--
-- 'userName', 'accessKeyInfo_userName' - The name of the IAM user that the access key is associated with.
--
-- 'accessKeyId', 'accessKeyInfo_accessKeyId' - The ID for this access key.
--
-- 'status', 'accessKeyInfo_status' - The status of the access key. @Active@ means that the key is valid for
-- API calls, while @Inactive@ means it is not.
--
-- 'secretAccessKey', 'accessKeyInfo_secretAccessKey' - The secret key used to sign requests.
newAccessKeyInfo ::
  -- | 'userName'
  Prelude.Text ->
  -- | 'accessKeyId'
  Core.AccessKey ->
  -- | 'status'
  StatusType ->
  -- | 'secretAccessKey'
  Prelude.Text ->
  AccessKeyInfo
newAccessKeyInfo
  pUserName_
  pAccessKeyId_
  pStatus_
  pSecretAccessKey_ =
    AccessKeyInfo'
      { createDate = Prelude.Nothing,
        userName = pUserName_,
        accessKeyId = pAccessKeyId_,
        status = pStatus_,
        secretAccessKey =
          Data._Sensitive Lens.# pSecretAccessKey_
      }

-- | The date when the access key was created.
accessKeyInfo_createDate :: Lens.Lens' AccessKeyInfo (Prelude.Maybe Prelude.UTCTime)
accessKeyInfo_createDate = Lens.lens (\AccessKeyInfo' {createDate} -> createDate) (\s@AccessKeyInfo' {} a -> s {createDate = a} :: AccessKeyInfo) Prelude.. Lens.mapping Data._Time

-- | The name of the IAM user that the access key is associated with.
accessKeyInfo_userName :: Lens.Lens' AccessKeyInfo Prelude.Text
accessKeyInfo_userName = Lens.lens (\AccessKeyInfo' {userName} -> userName) (\s@AccessKeyInfo' {} a -> s {userName = a} :: AccessKeyInfo)

-- | The ID for this access key.
accessKeyInfo_accessKeyId :: Lens.Lens' AccessKeyInfo Core.AccessKey
accessKeyInfo_accessKeyId = Lens.lens (\AccessKeyInfo' {accessKeyId} -> accessKeyId) (\s@AccessKeyInfo' {} a -> s {accessKeyId = a} :: AccessKeyInfo)

-- | The status of the access key. @Active@ means that the key is valid for
-- API calls, while @Inactive@ means it is not.
accessKeyInfo_status :: Lens.Lens' AccessKeyInfo StatusType
accessKeyInfo_status = Lens.lens (\AccessKeyInfo' {status} -> status) (\s@AccessKeyInfo' {} a -> s {status = a} :: AccessKeyInfo)

-- | The secret key used to sign requests.
accessKeyInfo_secretAccessKey :: Lens.Lens' AccessKeyInfo Prelude.Text
accessKeyInfo_secretAccessKey = Lens.lens (\AccessKeyInfo' {secretAccessKey} -> secretAccessKey) (\s@AccessKeyInfo' {} a -> s {secretAccessKey = a} :: AccessKeyInfo) Prelude.. Data._Sensitive

instance Data.FromXML AccessKeyInfo where
  parseXML x =
    AccessKeyInfo'
      Prelude.<$> (x Data..@? "CreateDate")
      Prelude.<*> (x Data..@ "UserName")
      Prelude.<*> (x Data..@ "AccessKeyId")
      Prelude.<*> (x Data..@ "Status")
      Prelude.<*> (x Data..@ "SecretAccessKey")

instance Prelude.Hashable AccessKeyInfo where
  hashWithSalt _salt AccessKeyInfo' {..} =
    _salt
      `Prelude.hashWithSalt` createDate
      `Prelude.hashWithSalt` userName
      `Prelude.hashWithSalt` accessKeyId
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` secretAccessKey

instance Prelude.NFData AccessKeyInfo where
  rnf AccessKeyInfo' {..} =
    Prelude.rnf createDate
      `Prelude.seq` Prelude.rnf userName
      `Prelude.seq` Prelude.rnf accessKeyId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf secretAccessKey
