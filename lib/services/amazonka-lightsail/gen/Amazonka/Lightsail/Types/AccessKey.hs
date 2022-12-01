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
-- Module      : Amazonka.Lightsail.Types.AccessKey
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.AccessKey where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Lightsail.Types.AccessKeyLastUsed
import Amazonka.Lightsail.Types.StatusType
import qualified Amazonka.Prelude as Prelude

-- | Describes an access key for an Amazon Lightsail bucket.
--
-- Access keys grant full programmatic access to the specified bucket and
-- its objects. You can have a maximum of two access keys per bucket. Use
-- the
-- <https://docs.aws.amazon.com/lightsail/2016-11-28/api-reference/API_CreateBucketAccessKey.html CreateBucketAccessKey>
-- action to create an access key for a specific bucket. For more
-- information about access keys, see
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-creating-bucket-access-keys Creating access keys for a bucket in Amazon Lightsail>
-- in the /Amazon Lightsail Developer Guide/.
--
-- The @secretAccessKey@ value is returned only in response to the
-- @CreateBucketAccessKey@ action. You can get a secret access key only
-- when you first create an access key; you cannot get the secret access
-- key later. If you lose the secret access key, you must create a new
-- access key.
--
-- /See:/ 'newAccessKey' smart constructor.
data AccessKey = AccessKey'
  { -- | The status of the access key.
    --
    -- A status of @Active@ means that the key is valid, while @Inactive@ means
    -- it is not.
    status :: Prelude.Maybe StatusType,
    -- | The secret access key used to sign requests.
    --
    -- You should store the secret access key in a safe location. We recommend
    -- that you delete the access key if the secret access key is compromised.
    secretAccessKey :: Prelude.Maybe Prelude.Text,
    -- | An object that describes the last time the access key was used.
    --
    -- This object does not include data in the response of a
    -- <https://docs.aws.amazon.com/lightsail/2016-11-28/api-reference/API_CreateBucketAccessKey.html CreateBucketAccessKey>
    -- action. If the access key has not been used, the @region@ and
    -- @serviceName@ values are @N\/A@, and the @lastUsedDate@ value is null.
    lastUsed :: Prelude.Maybe AccessKeyLastUsed,
    -- | The timestamp when the access key was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The ID of the access key.
    accessKeyId :: Prelude.Maybe (Core.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccessKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'accessKey_status' - The status of the access key.
--
-- A status of @Active@ means that the key is valid, while @Inactive@ means
-- it is not.
--
-- 'secretAccessKey', 'accessKey_secretAccessKey' - The secret access key used to sign requests.
--
-- You should store the secret access key in a safe location. We recommend
-- that you delete the access key if the secret access key is compromised.
--
-- 'lastUsed', 'accessKey_lastUsed' - An object that describes the last time the access key was used.
--
-- This object does not include data in the response of a
-- <https://docs.aws.amazon.com/lightsail/2016-11-28/api-reference/API_CreateBucketAccessKey.html CreateBucketAccessKey>
-- action. If the access key has not been used, the @region@ and
-- @serviceName@ values are @N\/A@, and the @lastUsedDate@ value is null.
--
-- 'createdAt', 'accessKey_createdAt' - The timestamp when the access key was created.
--
-- 'accessKeyId', 'accessKey_accessKeyId' - The ID of the access key.
newAccessKey ::
  AccessKey
newAccessKey =
  AccessKey'
    { status = Prelude.Nothing,
      secretAccessKey = Prelude.Nothing,
      lastUsed = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      accessKeyId = Prelude.Nothing
    }

-- | The status of the access key.
--
-- A status of @Active@ means that the key is valid, while @Inactive@ means
-- it is not.
accessKey_status :: Lens.Lens' AccessKey (Prelude.Maybe StatusType)
accessKey_status = Lens.lens (\AccessKey' {status} -> status) (\s@AccessKey' {} a -> s {status = a} :: AccessKey)

-- | The secret access key used to sign requests.
--
-- You should store the secret access key in a safe location. We recommend
-- that you delete the access key if the secret access key is compromised.
accessKey_secretAccessKey :: Lens.Lens' AccessKey (Prelude.Maybe Prelude.Text)
accessKey_secretAccessKey = Lens.lens (\AccessKey' {secretAccessKey} -> secretAccessKey) (\s@AccessKey' {} a -> s {secretAccessKey = a} :: AccessKey)

-- | An object that describes the last time the access key was used.
--
-- This object does not include data in the response of a
-- <https://docs.aws.amazon.com/lightsail/2016-11-28/api-reference/API_CreateBucketAccessKey.html CreateBucketAccessKey>
-- action. If the access key has not been used, the @region@ and
-- @serviceName@ values are @N\/A@, and the @lastUsedDate@ value is null.
accessKey_lastUsed :: Lens.Lens' AccessKey (Prelude.Maybe AccessKeyLastUsed)
accessKey_lastUsed = Lens.lens (\AccessKey' {lastUsed} -> lastUsed) (\s@AccessKey' {} a -> s {lastUsed = a} :: AccessKey)

-- | The timestamp when the access key was created.
accessKey_createdAt :: Lens.Lens' AccessKey (Prelude.Maybe Prelude.UTCTime)
accessKey_createdAt = Lens.lens (\AccessKey' {createdAt} -> createdAt) (\s@AccessKey' {} a -> s {createdAt = a} :: AccessKey) Prelude.. Lens.mapping Core._Time

-- | The ID of the access key.
accessKey_accessKeyId :: Lens.Lens' AccessKey (Prelude.Maybe Prelude.Text)
accessKey_accessKeyId = Lens.lens (\AccessKey' {accessKeyId} -> accessKeyId) (\s@AccessKey' {} a -> s {accessKeyId = a} :: AccessKey) Prelude.. Lens.mapping Core._Sensitive

instance Core.FromJSON AccessKey where
  parseJSON =
    Core.withObject
      "AccessKey"
      ( \x ->
          AccessKey'
            Prelude.<$> (x Core..:? "status")
            Prelude.<*> (x Core..:? "secretAccessKey")
            Prelude.<*> (x Core..:? "lastUsed")
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "accessKeyId")
      )

instance Prelude.Hashable AccessKey where
  hashWithSalt _salt AccessKey' {..} =
    _salt `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` secretAccessKey
      `Prelude.hashWithSalt` lastUsed
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` accessKeyId

instance Prelude.NFData AccessKey where
  rnf AccessKey' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf secretAccessKey
      `Prelude.seq` Prelude.rnf lastUsed
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf accessKeyId
