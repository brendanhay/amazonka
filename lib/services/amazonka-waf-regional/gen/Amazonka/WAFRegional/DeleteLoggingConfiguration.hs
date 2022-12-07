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
-- Module      : Amazonka.WAFRegional.DeleteLoggingConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is __AWS WAF Classic__ documentation. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html AWS WAF Classic>
-- in the developer guide.
--
-- __For the latest version of AWS WAF__, use the AWS WAFV2 API and see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html AWS WAF Developer Guide>.
-- With the latest version, AWS WAF has a single set of endpoints for
-- regional and global use.
--
-- Permanently deletes the LoggingConfiguration from the specified web ACL.
module Amazonka.WAFRegional.DeleteLoggingConfiguration
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFRegional.Types

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

instance Data.ToHeaders DeleteLoggingConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_Regional_20161128.DeleteLoggingConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteLoggingConfiguration where
  toJSON DeleteLoggingConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ResourceArn" Data..= resourceArn)]
      )

instance Data.ToPath DeleteLoggingConfiguration where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteLoggingConfiguration where
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
