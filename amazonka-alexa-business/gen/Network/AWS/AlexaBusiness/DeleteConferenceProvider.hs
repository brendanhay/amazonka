{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.AlexaBusiness.DeleteConferenceProvider
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a conference provider.
module Network.AWS.AlexaBusiness.DeleteConferenceProvider
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

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteConferenceProvider' smart constructor.
data DeleteConferenceProvider = DeleteConferenceProvider'
  { -- | The ARN of the conference provider.
    conferenceProviderArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DeleteConferenceProvider where
  type
    Rs DeleteConferenceProvider =
      DeleteConferenceProviderResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteConferenceProviderResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteConferenceProvider

instance Prelude.NFData DeleteConferenceProvider

instance Prelude.ToHeaders DeleteConferenceProvider where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AlexaForBusiness.DeleteConferenceProvider" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteConferenceProvider where
  toJSON DeleteConferenceProvider' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ConferenceProviderArn"
                  Prelude..= conferenceProviderArn
              )
          ]
      )

instance Prelude.ToPath DeleteConferenceProvider where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteConferenceProvider where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteConferenceProviderResponse' smart constructor.
data DeleteConferenceProviderResponse = DeleteConferenceProviderResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
