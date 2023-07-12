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
-- Module      : Amazonka.Panorama.RemoveApplicationInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes an application instance.
module Amazonka.Panorama.RemoveApplicationInstance
  ( -- * Creating a Request
    RemoveApplicationInstance (..),
    newRemoveApplicationInstance,

    -- * Request Lenses
    removeApplicationInstance_applicationInstanceId,

    -- * Destructuring the Response
    RemoveApplicationInstanceResponse (..),
    newRemoveApplicationInstanceResponse,

    -- * Response Lenses
    removeApplicationInstanceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Panorama.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRemoveApplicationInstance' smart constructor.
data RemoveApplicationInstance = RemoveApplicationInstance'
  { -- | An application instance ID.
    applicationInstanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveApplicationInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationInstanceId', 'removeApplicationInstance_applicationInstanceId' - An application instance ID.
newRemoveApplicationInstance ::
  -- | 'applicationInstanceId'
  Prelude.Text ->
  RemoveApplicationInstance
newRemoveApplicationInstance pApplicationInstanceId_ =
  RemoveApplicationInstance'
    { applicationInstanceId =
        pApplicationInstanceId_
    }

-- | An application instance ID.
removeApplicationInstance_applicationInstanceId :: Lens.Lens' RemoveApplicationInstance Prelude.Text
removeApplicationInstance_applicationInstanceId = Lens.lens (\RemoveApplicationInstance' {applicationInstanceId} -> applicationInstanceId) (\s@RemoveApplicationInstance' {} a -> s {applicationInstanceId = a} :: RemoveApplicationInstance)

instance Core.AWSRequest RemoveApplicationInstance where
  type
    AWSResponse RemoveApplicationInstance =
      RemoveApplicationInstanceResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          RemoveApplicationInstanceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RemoveApplicationInstance where
  hashWithSalt _salt RemoveApplicationInstance' {..} =
    _salt `Prelude.hashWithSalt` applicationInstanceId

instance Prelude.NFData RemoveApplicationInstance where
  rnf RemoveApplicationInstance' {..} =
    Prelude.rnf applicationInstanceId

instance Data.ToHeaders RemoveApplicationInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath RemoveApplicationInstance where
  toPath RemoveApplicationInstance' {..} =
    Prelude.mconcat
      [ "/application-instances/",
        Data.toBS applicationInstanceId
      ]

instance Data.ToQuery RemoveApplicationInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRemoveApplicationInstanceResponse' smart constructor.
data RemoveApplicationInstanceResponse = RemoveApplicationInstanceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveApplicationInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'removeApplicationInstanceResponse_httpStatus' - The response's http status code.
newRemoveApplicationInstanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RemoveApplicationInstanceResponse
newRemoveApplicationInstanceResponse pHttpStatus_ =
  RemoveApplicationInstanceResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
removeApplicationInstanceResponse_httpStatus :: Lens.Lens' RemoveApplicationInstanceResponse Prelude.Int
removeApplicationInstanceResponse_httpStatus = Lens.lens (\RemoveApplicationInstanceResponse' {httpStatus} -> httpStatus) (\s@RemoveApplicationInstanceResponse' {} a -> s {httpStatus = a} :: RemoveApplicationInstanceResponse)

instance
  Prelude.NFData
    RemoveApplicationInstanceResponse
  where
  rnf RemoveApplicationInstanceResponse' {..} =
    Prelude.rnf httpStatus
