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
-- Module      : Amazonka.DirectoryService.GetDirectoryLimits
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Obtains directory limit information for the current Region.
module Amazonka.DirectoryService.GetDirectoryLimits
  ( -- * Creating a Request
    GetDirectoryLimits (..),
    newGetDirectoryLimits,

    -- * Destructuring the Response
    GetDirectoryLimitsResponse (..),
    newGetDirectoryLimitsResponse,

    -- * Response Lenses
    getDirectoryLimitsResponse_directoryLimits,
    getDirectoryLimitsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectoryService.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the inputs for the GetDirectoryLimits operation.
--
-- /See:/ 'newGetDirectoryLimits' smart constructor.
data GetDirectoryLimits = GetDirectoryLimits'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDirectoryLimits' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetDirectoryLimits ::
  GetDirectoryLimits
newGetDirectoryLimits = GetDirectoryLimits'

instance Core.AWSRequest GetDirectoryLimits where
  type
    AWSResponse GetDirectoryLimits =
      GetDirectoryLimitsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDirectoryLimitsResponse'
            Prelude.<$> (x Data..?> "DirectoryLimits")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDirectoryLimits where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GetDirectoryLimits where
  rnf _ = ()

instance Data.ToHeaders GetDirectoryLimits where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DirectoryService_20150416.GetDirectoryLimits" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetDirectoryLimits where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath GetDirectoryLimits where
  toPath = Prelude.const "/"

instance Data.ToQuery GetDirectoryLimits where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the results of the GetDirectoryLimits operation.
--
-- /See:/ 'newGetDirectoryLimitsResponse' smart constructor.
data GetDirectoryLimitsResponse = GetDirectoryLimitsResponse'
  { -- | A DirectoryLimits object that contains the directory limits for the
    -- current Region.
    directoryLimits :: Prelude.Maybe DirectoryLimits,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDirectoryLimitsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryLimits', 'getDirectoryLimitsResponse_directoryLimits' - A DirectoryLimits object that contains the directory limits for the
-- current Region.
--
-- 'httpStatus', 'getDirectoryLimitsResponse_httpStatus' - The response's http status code.
newGetDirectoryLimitsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDirectoryLimitsResponse
newGetDirectoryLimitsResponse pHttpStatus_ =
  GetDirectoryLimitsResponse'
    { directoryLimits =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A DirectoryLimits object that contains the directory limits for the
-- current Region.
getDirectoryLimitsResponse_directoryLimits :: Lens.Lens' GetDirectoryLimitsResponse (Prelude.Maybe DirectoryLimits)
getDirectoryLimitsResponse_directoryLimits = Lens.lens (\GetDirectoryLimitsResponse' {directoryLimits} -> directoryLimits) (\s@GetDirectoryLimitsResponse' {} a -> s {directoryLimits = a} :: GetDirectoryLimitsResponse)

-- | The response's http status code.
getDirectoryLimitsResponse_httpStatus :: Lens.Lens' GetDirectoryLimitsResponse Prelude.Int
getDirectoryLimitsResponse_httpStatus = Lens.lens (\GetDirectoryLimitsResponse' {httpStatus} -> httpStatus) (\s@GetDirectoryLimitsResponse' {} a -> s {httpStatus = a} :: GetDirectoryLimitsResponse)

instance Prelude.NFData GetDirectoryLimitsResponse where
  rnf GetDirectoryLimitsResponse' {..} =
    Prelude.rnf directoryLimits
      `Prelude.seq` Prelude.rnf httpStatus
