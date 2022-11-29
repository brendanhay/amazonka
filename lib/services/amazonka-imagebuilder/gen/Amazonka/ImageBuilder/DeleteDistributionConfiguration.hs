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
-- Module      : Amazonka.ImageBuilder.DeleteDistributionConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a distribution configuration.
module Amazonka.ImageBuilder.DeleteDistributionConfiguration
  ( -- * Creating a Request
    DeleteDistributionConfiguration (..),
    newDeleteDistributionConfiguration,

    -- * Request Lenses
    deleteDistributionConfiguration_distributionConfigurationArn,

    -- * Destructuring the Response
    DeleteDistributionConfigurationResponse (..),
    newDeleteDistributionConfigurationResponse,

    -- * Response Lenses
    deleteDistributionConfigurationResponse_requestId,
    deleteDistributionConfigurationResponse_distributionConfigurationArn,
    deleteDistributionConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ImageBuilder.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteDistributionConfiguration' smart constructor.
data DeleteDistributionConfiguration = DeleteDistributionConfiguration'
  { -- | The Amazon Resource Name (ARN) of the distribution configuration to
    -- delete.
    distributionConfigurationArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDistributionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'distributionConfigurationArn', 'deleteDistributionConfiguration_distributionConfigurationArn' - The Amazon Resource Name (ARN) of the distribution configuration to
-- delete.
newDeleteDistributionConfiguration ::
  -- | 'distributionConfigurationArn'
  Prelude.Text ->
  DeleteDistributionConfiguration
newDeleteDistributionConfiguration
  pDistributionConfigurationArn_ =
    DeleteDistributionConfiguration'
      { distributionConfigurationArn =
          pDistributionConfigurationArn_
      }

-- | The Amazon Resource Name (ARN) of the distribution configuration to
-- delete.
deleteDistributionConfiguration_distributionConfigurationArn :: Lens.Lens' DeleteDistributionConfiguration Prelude.Text
deleteDistributionConfiguration_distributionConfigurationArn = Lens.lens (\DeleteDistributionConfiguration' {distributionConfigurationArn} -> distributionConfigurationArn) (\s@DeleteDistributionConfiguration' {} a -> s {distributionConfigurationArn = a} :: DeleteDistributionConfiguration)

instance
  Core.AWSRequest
    DeleteDistributionConfiguration
  where
  type
    AWSResponse DeleteDistributionConfiguration =
      DeleteDistributionConfigurationResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteDistributionConfigurationResponse'
            Prelude.<$> (x Core..?> "requestId")
            Prelude.<*> (x Core..?> "distributionConfigurationArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteDistributionConfiguration
  where
  hashWithSalt
    _salt
    DeleteDistributionConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` distributionConfigurationArn

instance
  Prelude.NFData
    DeleteDistributionConfiguration
  where
  rnf DeleteDistributionConfiguration' {..} =
    Prelude.rnf distributionConfigurationArn

instance
  Core.ToHeaders
    DeleteDistributionConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteDistributionConfiguration where
  toPath =
    Prelude.const "/DeleteDistributionConfiguration"

instance Core.ToQuery DeleteDistributionConfiguration where
  toQuery DeleteDistributionConfiguration' {..} =
    Prelude.mconcat
      [ "distributionConfigurationArn"
          Core.=: distributionConfigurationArn
      ]

-- | /See:/ 'newDeleteDistributionConfigurationResponse' smart constructor.
data DeleteDistributionConfigurationResponse = DeleteDistributionConfigurationResponse'
  { -- | The request ID that uniquely identifies this request.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the distribution configuration that
    -- was deleted.
    distributionConfigurationArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDistributionConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestId', 'deleteDistributionConfigurationResponse_requestId' - The request ID that uniquely identifies this request.
--
-- 'distributionConfigurationArn', 'deleteDistributionConfigurationResponse_distributionConfigurationArn' - The Amazon Resource Name (ARN) of the distribution configuration that
-- was deleted.
--
-- 'httpStatus', 'deleteDistributionConfigurationResponse_httpStatus' - The response's http status code.
newDeleteDistributionConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteDistributionConfigurationResponse
newDeleteDistributionConfigurationResponse
  pHttpStatus_ =
    DeleteDistributionConfigurationResponse'
      { requestId =
          Prelude.Nothing,
        distributionConfigurationArn =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The request ID that uniquely identifies this request.
deleteDistributionConfigurationResponse_requestId :: Lens.Lens' DeleteDistributionConfigurationResponse (Prelude.Maybe Prelude.Text)
deleteDistributionConfigurationResponse_requestId = Lens.lens (\DeleteDistributionConfigurationResponse' {requestId} -> requestId) (\s@DeleteDistributionConfigurationResponse' {} a -> s {requestId = a} :: DeleteDistributionConfigurationResponse)

-- | The Amazon Resource Name (ARN) of the distribution configuration that
-- was deleted.
deleteDistributionConfigurationResponse_distributionConfigurationArn :: Lens.Lens' DeleteDistributionConfigurationResponse (Prelude.Maybe Prelude.Text)
deleteDistributionConfigurationResponse_distributionConfigurationArn = Lens.lens (\DeleteDistributionConfigurationResponse' {distributionConfigurationArn} -> distributionConfigurationArn) (\s@DeleteDistributionConfigurationResponse' {} a -> s {distributionConfigurationArn = a} :: DeleteDistributionConfigurationResponse)

-- | The response's http status code.
deleteDistributionConfigurationResponse_httpStatus :: Lens.Lens' DeleteDistributionConfigurationResponse Prelude.Int
deleteDistributionConfigurationResponse_httpStatus = Lens.lens (\DeleteDistributionConfigurationResponse' {httpStatus} -> httpStatus) (\s@DeleteDistributionConfigurationResponse' {} a -> s {httpStatus = a} :: DeleteDistributionConfigurationResponse)

instance
  Prelude.NFData
    DeleteDistributionConfigurationResponse
  where
  rnf DeleteDistributionConfigurationResponse' {..} =
    Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf distributionConfigurationArn
      `Prelude.seq` Prelude.rnf httpStatus
