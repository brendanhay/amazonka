{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Transfer.UpdateAgreement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates some of the parameters for an existing agreement. Provide the
-- @AgreementId@ and the @ServerId@ for the agreement that you want to
-- update, along with the new values for the parameters to update.
module Amazonka.Transfer.UpdateAgreement
  ( -- * Creating a Request
    UpdateAgreement (..),
    newUpdateAgreement,

    -- * Request Lenses
    updateAgreement_accessRole,
    updateAgreement_baseDirectory,
    updateAgreement_description,
    updateAgreement_localProfileId,
    updateAgreement_partnerProfileId,
    updateAgreement_status,
    updateAgreement_agreementId,
    updateAgreement_serverId,

    -- * Destructuring the Response
    UpdateAgreementResponse (..),
    newUpdateAgreementResponse,

    -- * Response Lenses
    updateAgreementResponse_httpStatus,
    updateAgreementResponse_agreementId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transfer.Types

-- | /See:/ 'newUpdateAgreement' smart constructor.
data UpdateAgreement = UpdateAgreement'
  { -- | With AS2, you can send files by calling @StartFileTransfer@ and
    -- specifying the file paths in the request parameter, @SendFilePaths@. We
    -- use the file’s parent directory (for example, for
    -- @--send-file-paths \/bucket\/dir\/file.txt@, parent directory is
    -- @\/bucket\/dir\/@) to temporarily store a processed AS2 message file,
    -- store the MDN when we receive them from the partner, and write a final
    -- JSON file containing relevant metadata of the transmission. So, the
    -- @AccessRole@ needs to provide read and write access to the parent
    -- directory of the file location used in the @StartFileTransfer@ request.
    -- Additionally, you need to provide read and write access to the parent
    -- directory of the files that you intend to send with @StartFileTransfer@.
    accessRole :: Prelude.Maybe Prelude.Text,
    -- | To change the landing directory (folder) for files that are transferred,
    -- provide the bucket folder that you want to use; for example,
    -- @\/@/@DOC-EXAMPLE-BUCKET@/@\/@/@home@/@\/@/@mydirectory@/@ @.
    baseDirectory :: Prelude.Maybe Prelude.Text,
    -- | To replace the existing description, provide a short description for the
    -- agreement.
    description :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the AS2 local profile.
    --
    -- To change the local profile identifier, provide a new value here.
    localProfileId :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the partner profile. To change the partner
    -- profile identifier, provide a new value here.
    partnerProfileId :: Prelude.Maybe Prelude.Text,
    -- | You can update the status for the agreement, either activating an
    -- inactive agreement or the reverse.
    status :: Prelude.Maybe AgreementStatusType,
    -- | A unique identifier for the agreement. This identifier is returned when
    -- you create an agreement.
    agreementId :: Prelude.Text,
    -- | A system-assigned unique identifier for a server instance. This is the
    -- specific server that the agreement uses.
    serverId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAgreement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessRole', 'updateAgreement_accessRole' - With AS2, you can send files by calling @StartFileTransfer@ and
-- specifying the file paths in the request parameter, @SendFilePaths@. We
-- use the file’s parent directory (for example, for
-- @--send-file-paths \/bucket\/dir\/file.txt@, parent directory is
-- @\/bucket\/dir\/@) to temporarily store a processed AS2 message file,
-- store the MDN when we receive them from the partner, and write a final
-- JSON file containing relevant metadata of the transmission. So, the
-- @AccessRole@ needs to provide read and write access to the parent
-- directory of the file location used in the @StartFileTransfer@ request.
-- Additionally, you need to provide read and write access to the parent
-- directory of the files that you intend to send with @StartFileTransfer@.
--
-- 'baseDirectory', 'updateAgreement_baseDirectory' - To change the landing directory (folder) for files that are transferred,
-- provide the bucket folder that you want to use; for example,
-- @\/@/@DOC-EXAMPLE-BUCKET@/@\/@/@home@/@\/@/@mydirectory@/@ @.
--
-- 'description', 'updateAgreement_description' - To replace the existing description, provide a short description for the
-- agreement.
--
-- 'localProfileId', 'updateAgreement_localProfileId' - A unique identifier for the AS2 local profile.
--
-- To change the local profile identifier, provide a new value here.
--
-- 'partnerProfileId', 'updateAgreement_partnerProfileId' - A unique identifier for the partner profile. To change the partner
-- profile identifier, provide a new value here.
--
-- 'status', 'updateAgreement_status' - You can update the status for the agreement, either activating an
-- inactive agreement or the reverse.
--
-- 'agreementId', 'updateAgreement_agreementId' - A unique identifier for the agreement. This identifier is returned when
-- you create an agreement.
--
-- 'serverId', 'updateAgreement_serverId' - A system-assigned unique identifier for a server instance. This is the
-- specific server that the agreement uses.
newUpdateAgreement ::
  -- | 'agreementId'
  Prelude.Text ->
  -- | 'serverId'
  Prelude.Text ->
  UpdateAgreement
newUpdateAgreement pAgreementId_ pServerId_ =
  UpdateAgreement'
    { accessRole = Prelude.Nothing,
      baseDirectory = Prelude.Nothing,
      description = Prelude.Nothing,
      localProfileId = Prelude.Nothing,
      partnerProfileId = Prelude.Nothing,
      status = Prelude.Nothing,
      agreementId = pAgreementId_,
      serverId = pServerId_
    }

-- | With AS2, you can send files by calling @StartFileTransfer@ and
-- specifying the file paths in the request parameter, @SendFilePaths@. We
-- use the file’s parent directory (for example, for
-- @--send-file-paths \/bucket\/dir\/file.txt@, parent directory is
-- @\/bucket\/dir\/@) to temporarily store a processed AS2 message file,
-- store the MDN when we receive them from the partner, and write a final
-- JSON file containing relevant metadata of the transmission. So, the
-- @AccessRole@ needs to provide read and write access to the parent
-- directory of the file location used in the @StartFileTransfer@ request.
-- Additionally, you need to provide read and write access to the parent
-- directory of the files that you intend to send with @StartFileTransfer@.
updateAgreement_accessRole :: Lens.Lens' UpdateAgreement (Prelude.Maybe Prelude.Text)
updateAgreement_accessRole = Lens.lens (\UpdateAgreement' {accessRole} -> accessRole) (\s@UpdateAgreement' {} a -> s {accessRole = a} :: UpdateAgreement)

-- | To change the landing directory (folder) for files that are transferred,
-- provide the bucket folder that you want to use; for example,
-- @\/@/@DOC-EXAMPLE-BUCKET@/@\/@/@home@/@\/@/@mydirectory@/@ @.
updateAgreement_baseDirectory :: Lens.Lens' UpdateAgreement (Prelude.Maybe Prelude.Text)
updateAgreement_baseDirectory = Lens.lens (\UpdateAgreement' {baseDirectory} -> baseDirectory) (\s@UpdateAgreement' {} a -> s {baseDirectory = a} :: UpdateAgreement)

-- | To replace the existing description, provide a short description for the
-- agreement.
updateAgreement_description :: Lens.Lens' UpdateAgreement (Prelude.Maybe Prelude.Text)
updateAgreement_description = Lens.lens (\UpdateAgreement' {description} -> description) (\s@UpdateAgreement' {} a -> s {description = a} :: UpdateAgreement)

-- | A unique identifier for the AS2 local profile.
--
-- To change the local profile identifier, provide a new value here.
updateAgreement_localProfileId :: Lens.Lens' UpdateAgreement (Prelude.Maybe Prelude.Text)
updateAgreement_localProfileId = Lens.lens (\UpdateAgreement' {localProfileId} -> localProfileId) (\s@UpdateAgreement' {} a -> s {localProfileId = a} :: UpdateAgreement)

-- | A unique identifier for the partner profile. To change the partner
-- profile identifier, provide a new value here.
updateAgreement_partnerProfileId :: Lens.Lens' UpdateAgreement (Prelude.Maybe Prelude.Text)
updateAgreement_partnerProfileId = Lens.lens (\UpdateAgreement' {partnerProfileId} -> partnerProfileId) (\s@UpdateAgreement' {} a -> s {partnerProfileId = a} :: UpdateAgreement)

-- | You can update the status for the agreement, either activating an
-- inactive agreement or the reverse.
updateAgreement_status :: Lens.Lens' UpdateAgreement (Prelude.Maybe AgreementStatusType)
updateAgreement_status = Lens.lens (\UpdateAgreement' {status} -> status) (\s@UpdateAgreement' {} a -> s {status = a} :: UpdateAgreement)

-- | A unique identifier for the agreement. This identifier is returned when
-- you create an agreement.
updateAgreement_agreementId :: Lens.Lens' UpdateAgreement Prelude.Text
updateAgreement_agreementId = Lens.lens (\UpdateAgreement' {agreementId} -> agreementId) (\s@UpdateAgreement' {} a -> s {agreementId = a} :: UpdateAgreement)

-- | A system-assigned unique identifier for a server instance. This is the
-- specific server that the agreement uses.
updateAgreement_serverId :: Lens.Lens' UpdateAgreement Prelude.Text
updateAgreement_serverId = Lens.lens (\UpdateAgreement' {serverId} -> serverId) (\s@UpdateAgreement' {} a -> s {serverId = a} :: UpdateAgreement)

instance Core.AWSRequest UpdateAgreement where
  type
    AWSResponse UpdateAgreement =
      UpdateAgreementResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateAgreementResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "AgreementId")
      )

instance Prelude.Hashable UpdateAgreement where
  hashWithSalt _salt UpdateAgreement' {..} =
    _salt
      `Prelude.hashWithSalt` accessRole
      `Prelude.hashWithSalt` baseDirectory
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` localProfileId
      `Prelude.hashWithSalt` partnerProfileId
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` agreementId
      `Prelude.hashWithSalt` serverId

instance Prelude.NFData UpdateAgreement where
  rnf UpdateAgreement' {..} =
    Prelude.rnf accessRole
      `Prelude.seq` Prelude.rnf baseDirectory
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf localProfileId
      `Prelude.seq` Prelude.rnf partnerProfileId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf agreementId
      `Prelude.seq` Prelude.rnf serverId

instance Data.ToHeaders UpdateAgreement where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "TransferService.UpdateAgreement" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateAgreement where
  toJSON UpdateAgreement' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AccessRole" Data..=) Prelude.<$> accessRole,
            ("BaseDirectory" Data..=) Prelude.<$> baseDirectory,
            ("Description" Data..=) Prelude.<$> description,
            ("LocalProfileId" Data..=)
              Prelude.<$> localProfileId,
            ("PartnerProfileId" Data..=)
              Prelude.<$> partnerProfileId,
            ("Status" Data..=) Prelude.<$> status,
            Prelude.Just ("AgreementId" Data..= agreementId),
            Prelude.Just ("ServerId" Data..= serverId)
          ]
      )

instance Data.ToPath UpdateAgreement where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateAgreement where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAgreementResponse' smart constructor.
data UpdateAgreementResponse = UpdateAgreementResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A unique identifier for the agreement. This identifier is returned when
    -- you create an agreement.
    agreementId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAgreementResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateAgreementResponse_httpStatus' - The response's http status code.
--
-- 'agreementId', 'updateAgreementResponse_agreementId' - A unique identifier for the agreement. This identifier is returned when
-- you create an agreement.
newUpdateAgreementResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'agreementId'
  Prelude.Text ->
  UpdateAgreementResponse
newUpdateAgreementResponse pHttpStatus_ pAgreementId_ =
  UpdateAgreementResponse'
    { httpStatus = pHttpStatus_,
      agreementId = pAgreementId_
    }

-- | The response's http status code.
updateAgreementResponse_httpStatus :: Lens.Lens' UpdateAgreementResponse Prelude.Int
updateAgreementResponse_httpStatus = Lens.lens (\UpdateAgreementResponse' {httpStatus} -> httpStatus) (\s@UpdateAgreementResponse' {} a -> s {httpStatus = a} :: UpdateAgreementResponse)

-- | A unique identifier for the agreement. This identifier is returned when
-- you create an agreement.
updateAgreementResponse_agreementId :: Lens.Lens' UpdateAgreementResponse Prelude.Text
updateAgreementResponse_agreementId = Lens.lens (\UpdateAgreementResponse' {agreementId} -> agreementId) (\s@UpdateAgreementResponse' {} a -> s {agreementId = a} :: UpdateAgreementResponse)

instance Prelude.NFData UpdateAgreementResponse where
  rnf UpdateAgreementResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf agreementId
