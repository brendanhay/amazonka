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
-- Module      : Network.AWS.Route53.DeleteQueryLoggingConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a configuration for DNS query logging. If you delete a
-- configuration, Amazon Route 53 stops sending query logs to CloudWatch
-- Logs. Route 53 doesn\'t delete any logs that are already in CloudWatch
-- Logs.
--
-- For more information about DNS query logs, see
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_CreateQueryLoggingConfig.html CreateQueryLoggingConfig>.
module Network.AWS.Route53.DeleteQueryLoggingConfig
  ( -- * Creating a Request
    DeleteQueryLoggingConfig (..),
    newDeleteQueryLoggingConfig,

    -- * Request Lenses
    deleteQueryLoggingConfig_id,

    -- * Destructuring the Response
    DeleteQueryLoggingConfigResponse (..),
    newDeleteQueryLoggingConfigResponse,

    -- * Response Lenses
    deleteQueryLoggingConfigResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53.Types

-- | /See:/ 'newDeleteQueryLoggingConfig' smart constructor.
data DeleteQueryLoggingConfig = DeleteQueryLoggingConfig'
  { -- | The ID of the configuration that you want to delete.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteQueryLoggingConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'deleteQueryLoggingConfig_id' - The ID of the configuration that you want to delete.
newDeleteQueryLoggingConfig ::
  -- | 'id'
  Prelude.Text ->
  DeleteQueryLoggingConfig
newDeleteQueryLoggingConfig pId_ =
  DeleteQueryLoggingConfig' {id = pId_}

-- | The ID of the configuration that you want to delete.
deleteQueryLoggingConfig_id :: Lens.Lens' DeleteQueryLoggingConfig Prelude.Text
deleteQueryLoggingConfig_id = Lens.lens (\DeleteQueryLoggingConfig' {id} -> id) (\s@DeleteQueryLoggingConfig' {} a -> s {id = a} :: DeleteQueryLoggingConfig)

instance Prelude.AWSRequest DeleteQueryLoggingConfig where
  type
    Rs DeleteQueryLoggingConfig =
      DeleteQueryLoggingConfigResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteQueryLoggingConfigResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteQueryLoggingConfig

instance Prelude.NFData DeleteQueryLoggingConfig

instance Prelude.ToHeaders DeleteQueryLoggingConfig where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteQueryLoggingConfig where
  toPath DeleteQueryLoggingConfig' {..} =
    Prelude.mconcat
      ["/2013-04-01/queryloggingconfig/", Prelude.toBS id]

instance Prelude.ToQuery DeleteQueryLoggingConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteQueryLoggingConfigResponse' smart constructor.
data DeleteQueryLoggingConfigResponse = DeleteQueryLoggingConfigResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteQueryLoggingConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteQueryLoggingConfigResponse_httpStatus' - The response's http status code.
newDeleteQueryLoggingConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteQueryLoggingConfigResponse
newDeleteQueryLoggingConfigResponse pHttpStatus_ =
  DeleteQueryLoggingConfigResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteQueryLoggingConfigResponse_httpStatus :: Lens.Lens' DeleteQueryLoggingConfigResponse Prelude.Int
deleteQueryLoggingConfigResponse_httpStatus = Lens.lens (\DeleteQueryLoggingConfigResponse' {httpStatus} -> httpStatus) (\s@DeleteQueryLoggingConfigResponse' {} a -> s {httpStatus = a} :: DeleteQueryLoggingConfigResponse)

instance
  Prelude.NFData
    DeleteQueryLoggingConfigResponse
