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
-- Module      : Amazonka.ImageBuilder.DeleteInfrastructureConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an infrastructure configuration.
module Amazonka.ImageBuilder.DeleteInfrastructureConfiguration
  ( -- * Creating a Request
    DeleteInfrastructureConfiguration (..),
    newDeleteInfrastructureConfiguration,

    -- * Request Lenses
    deleteInfrastructureConfiguration_infrastructureConfigurationArn,

    -- * Destructuring the Response
    DeleteInfrastructureConfigurationResponse (..),
    newDeleteInfrastructureConfigurationResponse,

    -- * Response Lenses
    deleteInfrastructureConfigurationResponse_infrastructureConfigurationArn,
    deleteInfrastructureConfigurationResponse_requestId,
    deleteInfrastructureConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteInfrastructureConfiguration' smart constructor.
data DeleteInfrastructureConfiguration = DeleteInfrastructureConfiguration'
  { -- | The Amazon Resource Name (ARN) of the infrastructure configuration to
    -- delete.
    infrastructureConfigurationArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteInfrastructureConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'infrastructureConfigurationArn', 'deleteInfrastructureConfiguration_infrastructureConfigurationArn' - The Amazon Resource Name (ARN) of the infrastructure configuration to
-- delete.
newDeleteInfrastructureConfiguration ::
  -- | 'infrastructureConfigurationArn'
  Prelude.Text ->
  DeleteInfrastructureConfiguration
newDeleteInfrastructureConfiguration
  pInfrastructureConfigurationArn_ =
    DeleteInfrastructureConfiguration'
      { infrastructureConfigurationArn =
          pInfrastructureConfigurationArn_
      }

-- | The Amazon Resource Name (ARN) of the infrastructure configuration to
-- delete.
deleteInfrastructureConfiguration_infrastructureConfigurationArn :: Lens.Lens' DeleteInfrastructureConfiguration Prelude.Text
deleteInfrastructureConfiguration_infrastructureConfigurationArn = Lens.lens (\DeleteInfrastructureConfiguration' {infrastructureConfigurationArn} -> infrastructureConfigurationArn) (\s@DeleteInfrastructureConfiguration' {} a -> s {infrastructureConfigurationArn = a} :: DeleteInfrastructureConfiguration)

instance
  Core.AWSRequest
    DeleteInfrastructureConfiguration
  where
  type
    AWSResponse DeleteInfrastructureConfiguration =
      DeleteInfrastructureConfigurationResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteInfrastructureConfigurationResponse'
            Prelude.<$> (x Data..?> "infrastructureConfigurationArn")
              Prelude.<*> (x Data..?> "requestId")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteInfrastructureConfiguration
  where
  hashWithSalt
    _salt
    DeleteInfrastructureConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` infrastructureConfigurationArn

instance
  Prelude.NFData
    DeleteInfrastructureConfiguration
  where
  rnf DeleteInfrastructureConfiguration' {..} =
    Prelude.rnf infrastructureConfigurationArn

instance
  Data.ToHeaders
    DeleteInfrastructureConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToPath
    DeleteInfrastructureConfiguration
  where
  toPath =
    Prelude.const "/DeleteInfrastructureConfiguration"

instance
  Data.ToQuery
    DeleteInfrastructureConfiguration
  where
  toQuery DeleteInfrastructureConfiguration' {..} =
    Prelude.mconcat
      [ "infrastructureConfigurationArn"
          Data.=: infrastructureConfigurationArn
      ]

-- | /See:/ 'newDeleteInfrastructureConfigurationResponse' smart constructor.
data DeleteInfrastructureConfigurationResponse = DeleteInfrastructureConfigurationResponse'
  { -- | The Amazon Resource Name (ARN) of the infrastructure configuration that
    -- was deleted.
    infrastructureConfigurationArn :: Prelude.Maybe Prelude.Text,
    -- | The request ID that uniquely identifies this request.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteInfrastructureConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'infrastructureConfigurationArn', 'deleteInfrastructureConfigurationResponse_infrastructureConfigurationArn' - The Amazon Resource Name (ARN) of the infrastructure configuration that
-- was deleted.
--
-- 'requestId', 'deleteInfrastructureConfigurationResponse_requestId' - The request ID that uniquely identifies this request.
--
-- 'httpStatus', 'deleteInfrastructureConfigurationResponse_httpStatus' - The response's http status code.
newDeleteInfrastructureConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteInfrastructureConfigurationResponse
newDeleteInfrastructureConfigurationResponse
  pHttpStatus_ =
    DeleteInfrastructureConfigurationResponse'
      { infrastructureConfigurationArn =
          Prelude.Nothing,
        requestId = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The Amazon Resource Name (ARN) of the infrastructure configuration that
-- was deleted.
deleteInfrastructureConfigurationResponse_infrastructureConfigurationArn :: Lens.Lens' DeleteInfrastructureConfigurationResponse (Prelude.Maybe Prelude.Text)
deleteInfrastructureConfigurationResponse_infrastructureConfigurationArn = Lens.lens (\DeleteInfrastructureConfigurationResponse' {infrastructureConfigurationArn} -> infrastructureConfigurationArn) (\s@DeleteInfrastructureConfigurationResponse' {} a -> s {infrastructureConfigurationArn = a} :: DeleteInfrastructureConfigurationResponse)

-- | The request ID that uniquely identifies this request.
deleteInfrastructureConfigurationResponse_requestId :: Lens.Lens' DeleteInfrastructureConfigurationResponse (Prelude.Maybe Prelude.Text)
deleteInfrastructureConfigurationResponse_requestId = Lens.lens (\DeleteInfrastructureConfigurationResponse' {requestId} -> requestId) (\s@DeleteInfrastructureConfigurationResponse' {} a -> s {requestId = a} :: DeleteInfrastructureConfigurationResponse)

-- | The response's http status code.
deleteInfrastructureConfigurationResponse_httpStatus :: Lens.Lens' DeleteInfrastructureConfigurationResponse Prelude.Int
deleteInfrastructureConfigurationResponse_httpStatus = Lens.lens (\DeleteInfrastructureConfigurationResponse' {httpStatus} -> httpStatus) (\s@DeleteInfrastructureConfigurationResponse' {} a -> s {httpStatus = a} :: DeleteInfrastructureConfigurationResponse)

instance
  Prelude.NFData
    DeleteInfrastructureConfigurationResponse
  where
  rnf DeleteInfrastructureConfigurationResponse' {..} =
    Prelude.rnf infrastructureConfigurationArn
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf httpStatus
