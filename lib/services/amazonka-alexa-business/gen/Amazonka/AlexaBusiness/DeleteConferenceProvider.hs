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
-- Module      : Amazonka.AlexaBusiness.DeleteConferenceProvider
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a conference provider.
module Amazonka.AlexaBusiness.DeleteConferenceProvider
  ( -- * Creating a Request
    DeleteConferenceProvider (..),
    newDeleteConferenceProvider,

    -- * Request Lenses
    deleteConferenceProvider_conferenceProviderArn,

    -- * Destructuring the Response
    DeleteConferenceProviderResponse (..),
    newDeleteConferenceProviderResponse,

    -- * Response Lenses
    deleteConferenceProviderResponse_httpStatus,
  )
where

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteConferenceProvider' smart constructor.
data DeleteConferenceProvider = DeleteConferenceProvider'
  { -- | The ARN of the conference provider.
    conferenceProviderArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteConferenceProvider' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conferenceProviderArn', 'deleteConferenceProvider_conferenceProviderArn' - The ARN of the conference provider.
newDeleteConferenceProvider ::
  -- | 'conferenceProviderArn'
  Prelude.Text ->
  DeleteConferenceProvider
newDeleteConferenceProvider pConferenceProviderArn_ =
  DeleteConferenceProvider'
    { conferenceProviderArn =
        pConferenceProviderArn_
    }

-- | The ARN of the conference provider.
deleteConferenceProvider_conferenceProviderArn :: Lens.Lens' DeleteConferenceProvider Prelude.Text
deleteConferenceProvider_conferenceProviderArn = Lens.lens (\DeleteConferenceProvider' {conferenceProviderArn} -> conferenceProviderArn) (\s@DeleteConferenceProvider' {} a -> s {conferenceProviderArn = a} :: DeleteConferenceProvider)

instance Core.AWSRequest DeleteConferenceProvider where
  type
    AWSResponse DeleteConferenceProvider =
      DeleteConferenceProviderResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteConferenceProviderResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteConferenceProvider where
  hashWithSalt _salt DeleteConferenceProvider' {..} =
    _salt `Prelude.hashWithSalt` conferenceProviderArn

instance Prelude.NFData DeleteConferenceProvider where
  rnf DeleteConferenceProvider' {..} =
    Prelude.rnf conferenceProviderArn

instance Data.ToHeaders DeleteConferenceProvider where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AlexaForBusiness.DeleteConferenceProvider" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteConferenceProvider where
  toJSON DeleteConferenceProvider' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ConferenceProviderArn"
                  Data..= conferenceProviderArn
              )
          ]
      )

instance Data.ToPath DeleteConferenceProvider where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteConferenceProvider where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteConferenceProviderResponse' smart constructor.
data DeleteConferenceProviderResponse = DeleteConferenceProviderResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteConferenceProviderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteConferenceProviderResponse_httpStatus' - The response's http status code.
newDeleteConferenceProviderResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteConferenceProviderResponse
newDeleteConferenceProviderResponse pHttpStatus_ =
  DeleteConferenceProviderResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteConferenceProviderResponse_httpStatus :: Lens.Lens' DeleteConferenceProviderResponse Prelude.Int
deleteConferenceProviderResponse_httpStatus = Lens.lens (\DeleteConferenceProviderResponse' {httpStatus} -> httpStatus) (\s@DeleteConferenceProviderResponse' {} a -> s {httpStatus = a} :: DeleteConferenceProviderResponse)

instance
  Prelude.NFData
    DeleteConferenceProviderResponse
  where
  rnf DeleteConferenceProviderResponse' {..} =
    Prelude.rnf httpStatus
