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
-- Module      : Amazonka.MGN.InitializeService
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initialize Application Migration Service.
module Amazonka.MGN.InitializeService
  ( -- * Creating a Request
    InitializeService (..),
    newInitializeService,

    -- * Destructuring the Response
    InitializeServiceResponse (..),
    newInitializeServiceResponse,

    -- * Response Lenses
    initializeServiceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newInitializeService' smart constructor.
data InitializeService = InitializeService'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InitializeService' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newInitializeService ::
  InitializeService
newInitializeService = InitializeService'

instance Core.AWSRequest InitializeService where
  type
    AWSResponse InitializeService =
      InitializeServiceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          InitializeServiceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable InitializeService where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData InitializeService where
  rnf _ = ()

instance Data.ToHeaders InitializeService where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON InitializeService where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath InitializeService where
  toPath = Prelude.const "/InitializeService"

instance Data.ToQuery InitializeService where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newInitializeServiceResponse' smart constructor.
data InitializeServiceResponse = InitializeServiceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InitializeServiceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'initializeServiceResponse_httpStatus' - The response's http status code.
newInitializeServiceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  InitializeServiceResponse
newInitializeServiceResponse pHttpStatus_ =
  InitializeServiceResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
initializeServiceResponse_httpStatus :: Lens.Lens' InitializeServiceResponse Prelude.Int
initializeServiceResponse_httpStatus = Lens.lens (\InitializeServiceResponse' {httpStatus} -> httpStatus) (\s@InitializeServiceResponse' {} a -> s {httpStatus = a} :: InitializeServiceResponse)

instance Prelude.NFData InitializeServiceResponse where
  rnf InitializeServiceResponse' {..} =
    Prelude.rnf httpStatus
