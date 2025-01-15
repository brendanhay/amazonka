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
-- Module      : Amazonka.MigrationHubConfig.GetHomeRegion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the calling account’s home region, if configured. This API is
-- used by other AWS services to determine the regional endpoint for
-- calling AWS Application Discovery Service and Migration Hub. You must
-- call @GetHomeRegion@ at least once before you call any other AWS
-- Application Discovery Service and AWS Migration Hub APIs, to obtain the
-- account\'s Migration Hub home region.
module Amazonka.MigrationHubConfig.GetHomeRegion
  ( -- * Creating a Request
    GetHomeRegion (..),
    newGetHomeRegion,

    -- * Destructuring the Response
    GetHomeRegionResponse (..),
    newGetHomeRegionResponse,

    -- * Response Lenses
    getHomeRegionResponse_homeRegion,
    getHomeRegionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubConfig.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetHomeRegion' smart constructor.
data GetHomeRegion = GetHomeRegion'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetHomeRegion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetHomeRegion ::
  GetHomeRegion
newGetHomeRegion = GetHomeRegion'

instance Core.AWSRequest GetHomeRegion where
  type
    AWSResponse GetHomeRegion =
      GetHomeRegionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetHomeRegionResponse'
            Prelude.<$> (x Data..?> "HomeRegion")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetHomeRegion where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GetHomeRegion where
  rnf _ = ()

instance Data.ToHeaders GetHomeRegion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSMigrationHubMultiAccountService.GetHomeRegion" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetHomeRegion where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath GetHomeRegion where
  toPath = Prelude.const "/"

instance Data.ToQuery GetHomeRegion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetHomeRegionResponse' smart constructor.
data GetHomeRegionResponse = GetHomeRegionResponse'
  { -- | The name of the home region of the calling account.
    homeRegion :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetHomeRegionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'homeRegion', 'getHomeRegionResponse_homeRegion' - The name of the home region of the calling account.
--
-- 'httpStatus', 'getHomeRegionResponse_httpStatus' - The response's http status code.
newGetHomeRegionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetHomeRegionResponse
newGetHomeRegionResponse pHttpStatus_ =
  GetHomeRegionResponse'
    { homeRegion =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the home region of the calling account.
getHomeRegionResponse_homeRegion :: Lens.Lens' GetHomeRegionResponse (Prelude.Maybe Prelude.Text)
getHomeRegionResponse_homeRegion = Lens.lens (\GetHomeRegionResponse' {homeRegion} -> homeRegion) (\s@GetHomeRegionResponse' {} a -> s {homeRegion = a} :: GetHomeRegionResponse)

-- | The response's http status code.
getHomeRegionResponse_httpStatus :: Lens.Lens' GetHomeRegionResponse Prelude.Int
getHomeRegionResponse_httpStatus = Lens.lens (\GetHomeRegionResponse' {httpStatus} -> httpStatus) (\s@GetHomeRegionResponse' {} a -> s {httpStatus = a} :: GetHomeRegionResponse)

instance Prelude.NFData GetHomeRegionResponse where
  rnf GetHomeRegionResponse' {..} =
    Prelude.rnf homeRegion `Prelude.seq`
      Prelude.rnf httpStatus
