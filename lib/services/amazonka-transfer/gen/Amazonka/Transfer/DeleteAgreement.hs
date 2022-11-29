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
-- Module      : Amazonka.Transfer.DeleteAgreement
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete the agreement that\'s specified in the provided @AgreementId@.
module Amazonka.Transfer.DeleteAgreement
  ( -- * Creating a Request
    DeleteAgreement (..),
    newDeleteAgreement,

    -- * Request Lenses
    deleteAgreement_agreementId,
    deleteAgreement_serverId,

    -- * Destructuring the Response
    DeleteAgreementResponse (..),
    newDeleteAgreementResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transfer.Types

-- | /See:/ 'newDeleteAgreement' smart constructor.
data DeleteAgreement = DeleteAgreement'
  { -- | A unique identifier for the agreement. This identifier is returned when
    -- you create an agreement.
    agreementId :: Prelude.Text,
    -- | The server identifier associated with the agreement that you are
    -- deleting.
    serverId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAgreement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'agreementId', 'deleteAgreement_agreementId' - A unique identifier for the agreement. This identifier is returned when
-- you create an agreement.
--
-- 'serverId', 'deleteAgreement_serverId' - The server identifier associated with the agreement that you are
-- deleting.
newDeleteAgreement ::
  -- | 'agreementId'
  Prelude.Text ->
  -- | 'serverId'
  Prelude.Text ->
  DeleteAgreement
newDeleteAgreement pAgreementId_ pServerId_ =
  DeleteAgreement'
    { agreementId = pAgreementId_,
      serverId = pServerId_
    }

-- | A unique identifier for the agreement. This identifier is returned when
-- you create an agreement.
deleteAgreement_agreementId :: Lens.Lens' DeleteAgreement Prelude.Text
deleteAgreement_agreementId = Lens.lens (\DeleteAgreement' {agreementId} -> agreementId) (\s@DeleteAgreement' {} a -> s {agreementId = a} :: DeleteAgreement)

-- | The server identifier associated with the agreement that you are
-- deleting.
deleteAgreement_serverId :: Lens.Lens' DeleteAgreement Prelude.Text
deleteAgreement_serverId = Lens.lens (\DeleteAgreement' {serverId} -> serverId) (\s@DeleteAgreement' {} a -> s {serverId = a} :: DeleteAgreement)

instance Core.AWSRequest DeleteAgreement where
  type
    AWSResponse DeleteAgreement =
      DeleteAgreementResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeleteAgreementResponse'

instance Prelude.Hashable DeleteAgreement where
  hashWithSalt _salt DeleteAgreement' {..} =
    _salt `Prelude.hashWithSalt` agreementId
      `Prelude.hashWithSalt` serverId

instance Prelude.NFData DeleteAgreement where
  rnf DeleteAgreement' {..} =
    Prelude.rnf agreementId
      `Prelude.seq` Prelude.rnf serverId

instance Core.ToHeaders DeleteAgreement where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "TransferService.DeleteAgreement" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteAgreement where
  toJSON DeleteAgreement' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("AgreementId" Core..= agreementId),
            Prelude.Just ("ServerId" Core..= serverId)
          ]
      )

instance Core.ToPath DeleteAgreement where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteAgreement where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAgreementResponse' smart constructor.
data DeleteAgreementResponse = DeleteAgreementResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAgreementResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteAgreementResponse ::
  DeleteAgreementResponse
newDeleteAgreementResponse = DeleteAgreementResponse'

instance Prelude.NFData DeleteAgreementResponse where
  rnf _ = ()
