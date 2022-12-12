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
-- Module      : Amazonka.SecurityLake.CreateDatalakeAutoEnable
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Automatically enable Security Lake in the specified Regions to begin
-- ingesting security data. When you choose to enable organization accounts
-- automatically, then Security Lake begins to enable new accounts as
-- member accounts as they are added to the organization. Security Lake
-- does not enable existing organization accounts that are not yet enabled.
module Amazonka.SecurityLake.CreateDatalakeAutoEnable
  ( -- * Creating a Request
    CreateDatalakeAutoEnable (..),
    newCreateDatalakeAutoEnable,

    -- * Request Lenses
    createDatalakeAutoEnable_configurationForNewAccounts,

    -- * Destructuring the Response
    CreateDatalakeAutoEnableResponse (..),
    newCreateDatalakeAutoEnableResponse,

    -- * Response Lenses
    createDatalakeAutoEnableResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityLake.Types

-- | /See:/ 'newCreateDatalakeAutoEnable' smart constructor.
data CreateDatalakeAutoEnable = CreateDatalakeAutoEnable'
  { -- | Enable Amazon Security Lake with the specified configurations settings
    -- to begin ingesting security data for new accounts in Security Lake.
    configurationForNewAccounts :: [AutoEnableNewRegionConfiguration]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDatalakeAutoEnable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationForNewAccounts', 'createDatalakeAutoEnable_configurationForNewAccounts' - Enable Amazon Security Lake with the specified configurations settings
-- to begin ingesting security data for new accounts in Security Lake.
newCreateDatalakeAutoEnable ::
  CreateDatalakeAutoEnable
newCreateDatalakeAutoEnable =
  CreateDatalakeAutoEnable'
    { configurationForNewAccounts =
        Prelude.mempty
    }

-- | Enable Amazon Security Lake with the specified configurations settings
-- to begin ingesting security data for new accounts in Security Lake.
createDatalakeAutoEnable_configurationForNewAccounts :: Lens.Lens' CreateDatalakeAutoEnable [AutoEnableNewRegionConfiguration]
createDatalakeAutoEnable_configurationForNewAccounts = Lens.lens (\CreateDatalakeAutoEnable' {configurationForNewAccounts} -> configurationForNewAccounts) (\s@CreateDatalakeAutoEnable' {} a -> s {configurationForNewAccounts = a} :: CreateDatalakeAutoEnable) Prelude.. Lens.coerced

instance Core.AWSRequest CreateDatalakeAutoEnable where
  type
    AWSResponse CreateDatalakeAutoEnable =
      CreateDatalakeAutoEnableResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateDatalakeAutoEnableResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDatalakeAutoEnable where
  hashWithSalt _salt CreateDatalakeAutoEnable' {..} =
    _salt
      `Prelude.hashWithSalt` configurationForNewAccounts

instance Prelude.NFData CreateDatalakeAutoEnable where
  rnf CreateDatalakeAutoEnable' {..} =
    Prelude.rnf configurationForNewAccounts

instance Data.ToHeaders CreateDatalakeAutoEnable where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateDatalakeAutoEnable where
  toJSON CreateDatalakeAutoEnable' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "configurationForNewAccounts"
                  Data..= configurationForNewAccounts
              )
          ]
      )

instance Data.ToPath CreateDatalakeAutoEnable where
  toPath = Prelude.const "/v1/datalake/autoenable"

instance Data.ToQuery CreateDatalakeAutoEnable where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDatalakeAutoEnableResponse' smart constructor.
data CreateDatalakeAutoEnableResponse = CreateDatalakeAutoEnableResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDatalakeAutoEnableResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createDatalakeAutoEnableResponse_httpStatus' - The response's http status code.
newCreateDatalakeAutoEnableResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDatalakeAutoEnableResponse
newCreateDatalakeAutoEnableResponse pHttpStatus_ =
  CreateDatalakeAutoEnableResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
createDatalakeAutoEnableResponse_httpStatus :: Lens.Lens' CreateDatalakeAutoEnableResponse Prelude.Int
createDatalakeAutoEnableResponse_httpStatus = Lens.lens (\CreateDatalakeAutoEnableResponse' {httpStatus} -> httpStatus) (\s@CreateDatalakeAutoEnableResponse' {} a -> s {httpStatus = a} :: CreateDatalakeAutoEnableResponse)

instance
  Prelude.NFData
    CreateDatalakeAutoEnableResponse
  where
  rnf CreateDatalakeAutoEnableResponse' {..} =
    Prelude.rnf httpStatus
