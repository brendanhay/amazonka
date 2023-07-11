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
-- Module      : Amazonka.VoiceId.Types.ServerSideEncryptionUpdateDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VoiceId.Types.ServerSideEncryptionUpdateDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.VoiceId.Types.ServerSideEncryptionUpdateStatus

-- | Details about the most recent server-side encryption configuration
-- update. When the server-side encryption configuration is changed,
-- dependency on the old KMS key is removed through an asynchronous
-- process. When this update is complete, the domain’s data can only be
-- accessed using the new KMS key.
--
-- /See:/ 'newServerSideEncryptionUpdateDetails' smart constructor.
data ServerSideEncryptionUpdateDetails = ServerSideEncryptionUpdateDetails'
  { -- | Message explaining the current UpdateStatus. When the UpdateStatus is
    -- FAILED, this message explains the cause of the failure.
    message :: Prelude.Maybe Prelude.Text,
    -- | The previous KMS key ID the domain was encrypted with, before
    -- ServerSideEncryptionConfiguration was updated to a new KMS key ID.
    oldKmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Status of the server-side encryption update. During an update, if there
    -- is an issue with the domain\'s current or old KMS key ID, such as an
    -- inaccessible or disabled key, then the status is FAILED. In order to
    -- resolve this, the key needs to be made accessible, and then an
    -- UpdateDomain call with the existing server-side encryption configuration
    -- will re-attempt this update process.
    updateStatus :: Prelude.Maybe ServerSideEncryptionUpdateStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServerSideEncryptionUpdateDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'serverSideEncryptionUpdateDetails_message' - Message explaining the current UpdateStatus. When the UpdateStatus is
-- FAILED, this message explains the cause of the failure.
--
-- 'oldKmsKeyId', 'serverSideEncryptionUpdateDetails_oldKmsKeyId' - The previous KMS key ID the domain was encrypted with, before
-- ServerSideEncryptionConfiguration was updated to a new KMS key ID.
--
-- 'updateStatus', 'serverSideEncryptionUpdateDetails_updateStatus' - Status of the server-side encryption update. During an update, if there
-- is an issue with the domain\'s current or old KMS key ID, such as an
-- inaccessible or disabled key, then the status is FAILED. In order to
-- resolve this, the key needs to be made accessible, and then an
-- UpdateDomain call with the existing server-side encryption configuration
-- will re-attempt this update process.
newServerSideEncryptionUpdateDetails ::
  ServerSideEncryptionUpdateDetails
newServerSideEncryptionUpdateDetails =
  ServerSideEncryptionUpdateDetails'
    { message =
        Prelude.Nothing,
      oldKmsKeyId = Prelude.Nothing,
      updateStatus = Prelude.Nothing
    }

-- | Message explaining the current UpdateStatus. When the UpdateStatus is
-- FAILED, this message explains the cause of the failure.
serverSideEncryptionUpdateDetails_message :: Lens.Lens' ServerSideEncryptionUpdateDetails (Prelude.Maybe Prelude.Text)
serverSideEncryptionUpdateDetails_message = Lens.lens (\ServerSideEncryptionUpdateDetails' {message} -> message) (\s@ServerSideEncryptionUpdateDetails' {} a -> s {message = a} :: ServerSideEncryptionUpdateDetails)

-- | The previous KMS key ID the domain was encrypted with, before
-- ServerSideEncryptionConfiguration was updated to a new KMS key ID.
serverSideEncryptionUpdateDetails_oldKmsKeyId :: Lens.Lens' ServerSideEncryptionUpdateDetails (Prelude.Maybe Prelude.Text)
serverSideEncryptionUpdateDetails_oldKmsKeyId = Lens.lens (\ServerSideEncryptionUpdateDetails' {oldKmsKeyId} -> oldKmsKeyId) (\s@ServerSideEncryptionUpdateDetails' {} a -> s {oldKmsKeyId = a} :: ServerSideEncryptionUpdateDetails)

-- | Status of the server-side encryption update. During an update, if there
-- is an issue with the domain\'s current or old KMS key ID, such as an
-- inaccessible or disabled key, then the status is FAILED. In order to
-- resolve this, the key needs to be made accessible, and then an
-- UpdateDomain call with the existing server-side encryption configuration
-- will re-attempt this update process.
serverSideEncryptionUpdateDetails_updateStatus :: Lens.Lens' ServerSideEncryptionUpdateDetails (Prelude.Maybe ServerSideEncryptionUpdateStatus)
serverSideEncryptionUpdateDetails_updateStatus = Lens.lens (\ServerSideEncryptionUpdateDetails' {updateStatus} -> updateStatus) (\s@ServerSideEncryptionUpdateDetails' {} a -> s {updateStatus = a} :: ServerSideEncryptionUpdateDetails)

instance
  Data.FromJSON
    ServerSideEncryptionUpdateDetails
  where
  parseJSON =
    Data.withObject
      "ServerSideEncryptionUpdateDetails"
      ( \x ->
          ServerSideEncryptionUpdateDetails'
            Prelude.<$> (x Data..:? "Message")
            Prelude.<*> (x Data..:? "OldKmsKeyId")
            Prelude.<*> (x Data..:? "UpdateStatus")
      )

instance
  Prelude.Hashable
    ServerSideEncryptionUpdateDetails
  where
  hashWithSalt
    _salt
    ServerSideEncryptionUpdateDetails' {..} =
      _salt
        `Prelude.hashWithSalt` message
        `Prelude.hashWithSalt` oldKmsKeyId
        `Prelude.hashWithSalt` updateStatus

instance
  Prelude.NFData
    ServerSideEncryptionUpdateDetails
  where
  rnf ServerSideEncryptionUpdateDetails' {..} =
    Prelude.rnf message
      `Prelude.seq` Prelude.rnf oldKmsKeyId
      `Prelude.seq` Prelude.rnf updateStatus
