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
-- Module      : Amazonka.IAM.Types.AccessKeyMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IAM.Types.AccessKeyMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types.StatusType
import qualified Amazonka.Prelude as Prelude

-- | Contains information about an Amazon Web Services access key, without
-- its secret key.
--
-- This data type is used as a response element in the ListAccessKeys
-- operation.
--
-- /See:/ 'newAccessKeyMetadata' smart constructor.
data AccessKeyMetadata = AccessKeyMetadata'
  { -- | The ID for this access key.
    accessKeyId :: Prelude.Maybe Core.AccessKey,
    -- | The date when the access key was created.
    createDate :: Prelude.Maybe Data.ISO8601,
    -- | The status of the access key. @Active@ means that the key is valid for
    -- API calls; @Inactive@ means it is not.
    status :: Prelude.Maybe StatusType,
    -- | The name of the IAM user that the key is associated with.
    userName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccessKeyMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessKeyId', 'accessKeyMetadata_accessKeyId' - The ID for this access key.
--
-- 'createDate', 'accessKeyMetadata_createDate' - The date when the access key was created.
--
-- 'status', 'accessKeyMetadata_status' - The status of the access key. @Active@ means that the key is valid for
-- API calls; @Inactive@ means it is not.
--
-- 'userName', 'accessKeyMetadata_userName' - The name of the IAM user that the key is associated with.
newAccessKeyMetadata ::
  AccessKeyMetadata
newAccessKeyMetadata =
  AccessKeyMetadata'
    { accessKeyId = Prelude.Nothing,
      createDate = Prelude.Nothing,
      status = Prelude.Nothing,
      userName = Prelude.Nothing
    }

-- | The ID for this access key.
accessKeyMetadata_accessKeyId :: Lens.Lens' AccessKeyMetadata (Prelude.Maybe Core.AccessKey)
accessKeyMetadata_accessKeyId = Lens.lens (\AccessKeyMetadata' {accessKeyId} -> accessKeyId) (\s@AccessKeyMetadata' {} a -> s {accessKeyId = a} :: AccessKeyMetadata)

-- | The date when the access key was created.
accessKeyMetadata_createDate :: Lens.Lens' AccessKeyMetadata (Prelude.Maybe Prelude.UTCTime)
accessKeyMetadata_createDate = Lens.lens (\AccessKeyMetadata' {createDate} -> createDate) (\s@AccessKeyMetadata' {} a -> s {createDate = a} :: AccessKeyMetadata) Prelude.. Lens.mapping Data._Time

-- | The status of the access key. @Active@ means that the key is valid for
-- API calls; @Inactive@ means it is not.
accessKeyMetadata_status :: Lens.Lens' AccessKeyMetadata (Prelude.Maybe StatusType)
accessKeyMetadata_status = Lens.lens (\AccessKeyMetadata' {status} -> status) (\s@AccessKeyMetadata' {} a -> s {status = a} :: AccessKeyMetadata)

-- | The name of the IAM user that the key is associated with.
accessKeyMetadata_userName :: Lens.Lens' AccessKeyMetadata (Prelude.Maybe Prelude.Text)
accessKeyMetadata_userName = Lens.lens (\AccessKeyMetadata' {userName} -> userName) (\s@AccessKeyMetadata' {} a -> s {userName = a} :: AccessKeyMetadata)

instance Data.FromXML AccessKeyMetadata where
  parseXML x =
    AccessKeyMetadata'
      Prelude.<$> (x Data..@? "AccessKeyId")
      Prelude.<*> (x Data..@? "CreateDate")
      Prelude.<*> (x Data..@? "Status")
      Prelude.<*> (x Data..@? "UserName")

instance Prelude.Hashable AccessKeyMetadata where
  hashWithSalt _salt AccessKeyMetadata' {..} =
    _salt `Prelude.hashWithSalt` accessKeyId
      `Prelude.hashWithSalt` createDate
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` userName

instance Prelude.NFData AccessKeyMetadata where
  rnf AccessKeyMetadata' {..} =
    Prelude.rnf accessKeyId
      `Prelude.seq` Prelude.rnf createDate
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf userName
