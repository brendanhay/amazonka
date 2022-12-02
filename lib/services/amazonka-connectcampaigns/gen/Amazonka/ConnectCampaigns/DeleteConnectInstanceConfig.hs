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
-- Module      : Amazonka.ConnectCampaigns.DeleteConnectInstanceConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a connect instance config from the specified AWS account.
module Amazonka.ConnectCampaigns.DeleteConnectInstanceConfig
  ( -- * Creating a Request
    DeleteConnectInstanceConfig (..),
    newDeleteConnectInstanceConfig,

    -- * Request Lenses
    deleteConnectInstanceConfig_connectInstanceId,

    -- * Destructuring the Response
    DeleteConnectInstanceConfigResponse (..),
    newDeleteConnectInstanceConfigResponse,
  )
where

import Amazonka.ConnectCampaigns.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | DeleteCampaignRequest
--
-- /See:/ 'newDeleteConnectInstanceConfig' smart constructor.
data DeleteConnectInstanceConfig = DeleteConnectInstanceConfig'
  { connectInstanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteConnectInstanceConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectInstanceId', 'deleteConnectInstanceConfig_connectInstanceId' - Undocumented member.
newDeleteConnectInstanceConfig ::
  -- | 'connectInstanceId'
  Prelude.Text ->
  DeleteConnectInstanceConfig
newDeleteConnectInstanceConfig pConnectInstanceId_ =
  DeleteConnectInstanceConfig'
    { connectInstanceId =
        pConnectInstanceId_
    }

-- | Undocumented member.
deleteConnectInstanceConfig_connectInstanceId :: Lens.Lens' DeleteConnectInstanceConfig Prelude.Text
deleteConnectInstanceConfig_connectInstanceId = Lens.lens (\DeleteConnectInstanceConfig' {connectInstanceId} -> connectInstanceId) (\s@DeleteConnectInstanceConfig' {} a -> s {connectInstanceId = a} :: DeleteConnectInstanceConfig)

instance Core.AWSRequest DeleteConnectInstanceConfig where
  type
    AWSResponse DeleteConnectInstanceConfig =
      DeleteConnectInstanceConfigResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DeleteConnectInstanceConfigResponse'

instance Prelude.Hashable DeleteConnectInstanceConfig where
  hashWithSalt _salt DeleteConnectInstanceConfig' {..} =
    _salt `Prelude.hashWithSalt` connectInstanceId

instance Prelude.NFData DeleteConnectInstanceConfig where
  rnf DeleteConnectInstanceConfig' {..} =
    Prelude.rnf connectInstanceId

instance Data.ToHeaders DeleteConnectInstanceConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteConnectInstanceConfig where
  toPath DeleteConnectInstanceConfig' {..} =
    Prelude.mconcat
      [ "/connect-instance/",
        Data.toBS connectInstanceId,
        "/config"
      ]

instance Data.ToQuery DeleteConnectInstanceConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteConnectInstanceConfigResponse' smart constructor.
data DeleteConnectInstanceConfigResponse = DeleteConnectInstanceConfigResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteConnectInstanceConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteConnectInstanceConfigResponse ::
  DeleteConnectInstanceConfigResponse
newDeleteConnectInstanceConfigResponse =
  DeleteConnectInstanceConfigResponse'

instance
  Prelude.NFData
    DeleteConnectInstanceConfigResponse
  where
  rnf _ = ()
