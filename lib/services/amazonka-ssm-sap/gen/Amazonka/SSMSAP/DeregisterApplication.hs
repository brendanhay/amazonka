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
-- Module      : Amazonka.SSMSAP.DeregisterApplication
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregister an SAP application with AWS Systems Manager for SAP. This
-- action does not aﬀect the existing setup of your SAP workloads on Amazon
-- EC2.
module Amazonka.SSMSAP.DeregisterApplication
  ( -- * Creating a Request
    DeregisterApplication (..),
    newDeregisterApplication,

    -- * Request Lenses
    deregisterApplication_applicationId,

    -- * Destructuring the Response
    DeregisterApplicationResponse (..),
    newDeregisterApplicationResponse,

    -- * Response Lenses
    deregisterApplicationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMSAP.Types

-- | /See:/ 'newDeregisterApplication' smart constructor.
data DeregisterApplication = DeregisterApplication'
  { applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'deregisterApplication_applicationId' -
newDeregisterApplication ::
  -- | 'applicationId'
  Prelude.Text ->
  DeregisterApplication
newDeregisterApplication pApplicationId_ =
  DeregisterApplication'
    { applicationId =
        pApplicationId_
    }

deregisterApplication_applicationId :: Lens.Lens' DeregisterApplication Prelude.Text
deregisterApplication_applicationId = Lens.lens (\DeregisterApplication' {applicationId} -> applicationId) (\s@DeregisterApplication' {} a -> s {applicationId = a} :: DeregisterApplication)

instance Core.AWSRequest DeregisterApplication where
  type
    AWSResponse DeregisterApplication =
      DeregisterApplicationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeregisterApplicationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeregisterApplication where
  hashWithSalt _salt DeregisterApplication' {..} =
    _salt `Prelude.hashWithSalt` applicationId

instance Prelude.NFData DeregisterApplication where
  rnf DeregisterApplication' {..} =
    Prelude.rnf applicationId

instance Data.ToHeaders DeregisterApplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeregisterApplication where
  toJSON DeregisterApplication' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ApplicationId" Data..= applicationId)
          ]
      )

instance Data.ToPath DeregisterApplication where
  toPath = Prelude.const "/deregister-application"

instance Data.ToQuery DeregisterApplication where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeregisterApplicationResponse' smart constructor.
data DeregisterApplicationResponse = DeregisterApplicationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deregisterApplicationResponse_httpStatus' - The response's http status code.
newDeregisterApplicationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeregisterApplicationResponse
newDeregisterApplicationResponse pHttpStatus_ =
  DeregisterApplicationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deregisterApplicationResponse_httpStatus :: Lens.Lens' DeregisterApplicationResponse Prelude.Int
deregisterApplicationResponse_httpStatus = Lens.lens (\DeregisterApplicationResponse' {httpStatus} -> httpStatus) (\s@DeregisterApplicationResponse' {} a -> s {httpStatus = a} :: DeregisterApplicationResponse)

instance Prelude.NFData DeregisterApplicationResponse where
  rnf DeregisterApplicationResponse' {..} =
    Prelude.rnf httpStatus
