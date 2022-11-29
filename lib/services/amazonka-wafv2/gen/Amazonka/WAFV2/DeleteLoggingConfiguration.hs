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
-- Module      : Amazonka.WAFV2.DeleteLoggingConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the LoggingConfiguration from the specified web ACL.
module Amazonka.WAFV2.DeleteLoggingConfiguration
  ( -- * Creating a Request
    DeleteLoggingConfiguration (..),
    newDeleteLoggingConfiguration,

    -- * Request Lenses
    deleteLoggingConfiguration_resourceArn,

    -- * Destructuring the Response
    DeleteLoggingConfigurationResponse (..),
    newDeleteLoggingConfigurationResponse,

    -- * Response Lenses
    deleteLoggingConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFV2.Types

-- | /See:/ 'newDeleteLoggingConfiguration' smart constructor.
data DeleteLoggingConfiguration = DeleteLoggingConfiguration'
  { -- | The Amazon Resource Name (ARN) of the web ACL from which you want to
    -- delete the LoggingConfiguration.
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLoggingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'deleteLoggingConfiguration_resourceArn' - The Amazon Resource Name (ARN) of the web ACL from which you want to
-- delete the LoggingConfiguration.
newDeleteLoggingConfiguration ::
  -- | 'resourceArn'
  Prelude.Text ->
  DeleteLoggingConfiguration
newDeleteLoggingConfiguration pResourceArn_ =
  DeleteLoggingConfiguration'
    { resourceArn =
        pResourceArn_
    }

-- | The Amazon Resource Name (ARN) of the web ACL from which you want to
-- delete the LoggingConfiguration.
deleteLoggingConfiguration_resourceArn :: Lens.Lens' DeleteLoggingConfiguration Prelude.Text
deleteLoggingConfiguration_resourceArn = Lens.lens (\DeleteLoggingConfiguration' {resourceArn} -> resourceArn) (\s@DeleteLoggingConfiguration' {} a -> s {resourceArn = a} :: DeleteLoggingConfiguration)

instance Core.AWSRequest DeleteLoggingConfiguration where
  type
    AWSResponse DeleteLoggingConfiguration =
      DeleteLoggingConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteLoggingConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteLoggingConfiguration where
  hashWithSalt _salt DeleteLoggingConfiguration' {..} =
    _salt `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData DeleteLoggingConfiguration where
  rnf DeleteLoggingConfiguration' {..} =
    Prelude.rnf resourceArn

instance Core.ToHeaders DeleteLoggingConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSWAF_20190729.DeleteLoggingConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteLoggingConfiguration where
  toJSON DeleteLoggingConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("ResourceArn" Core..= resourceArn)]
      )

instance Core.ToPath DeleteLoggingConfiguration where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteLoggingConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteLoggingConfigurationResponse' smart constructor.
data DeleteLoggingConfigurationResponse = DeleteLoggingConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLoggingConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteLoggingConfigurationResponse_httpStatus' - The response's http status code.
newDeleteLoggingConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteLoggingConfigurationResponse
newDeleteLoggingConfigurationResponse pHttpStatus_ =
  DeleteLoggingConfigurationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteLoggingConfigurationResponse_httpStatus :: Lens.Lens' DeleteLoggingConfigurationResponse Prelude.Int
deleteLoggingConfigurationResponse_httpStatus = Lens.lens (\DeleteLoggingConfigurationResponse' {httpStatus} -> httpStatus) (\s@DeleteLoggingConfigurationResponse' {} a -> s {httpStatus = a} :: DeleteLoggingConfigurationResponse)

instance
  Prelude.NFData
    DeleteLoggingConfigurationResponse
  where
  rnf DeleteLoggingConfigurationResponse' {..} =
    Prelude.rnf httpStatus
