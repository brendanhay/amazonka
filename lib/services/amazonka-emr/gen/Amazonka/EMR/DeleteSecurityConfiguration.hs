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
-- Module      : Amazonka.EMR.DeleteSecurityConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a security configuration.
module Amazonka.EMR.DeleteSecurityConfiguration
  ( -- * Creating a Request
    DeleteSecurityConfiguration (..),
    newDeleteSecurityConfiguration,

    -- * Request Lenses
    deleteSecurityConfiguration_name,

    -- * Destructuring the Response
    DeleteSecurityConfigurationResponse (..),
    newDeleteSecurityConfigurationResponse,

    -- * Response Lenses
    deleteSecurityConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteSecurityConfiguration' smart constructor.
data DeleteSecurityConfiguration = DeleteSecurityConfiguration'
  { -- | The name of the security configuration.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSecurityConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteSecurityConfiguration_name' - The name of the security configuration.
newDeleteSecurityConfiguration ::
  -- | 'name'
  Prelude.Text ->
  DeleteSecurityConfiguration
newDeleteSecurityConfiguration pName_ =
  DeleteSecurityConfiguration' {name = pName_}

-- | The name of the security configuration.
deleteSecurityConfiguration_name :: Lens.Lens' DeleteSecurityConfiguration Prelude.Text
deleteSecurityConfiguration_name = Lens.lens (\DeleteSecurityConfiguration' {name} -> name) (\s@DeleteSecurityConfiguration' {} a -> s {name = a} :: DeleteSecurityConfiguration)

instance Core.AWSRequest DeleteSecurityConfiguration where
  type
    AWSResponse DeleteSecurityConfiguration =
      DeleteSecurityConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteSecurityConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteSecurityConfiguration where
  hashWithSalt _salt DeleteSecurityConfiguration' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DeleteSecurityConfiguration where
  rnf DeleteSecurityConfiguration' {..} =
    Prelude.rnf name

instance Data.ToHeaders DeleteSecurityConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ElasticMapReduce.DeleteSecurityConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteSecurityConfiguration where
  toJSON DeleteSecurityConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Data..= name)]
      )

instance Data.ToPath DeleteSecurityConfiguration where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteSecurityConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteSecurityConfigurationResponse' smart constructor.
data DeleteSecurityConfigurationResponse = DeleteSecurityConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSecurityConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteSecurityConfigurationResponse_httpStatus' - The response's http status code.
newDeleteSecurityConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteSecurityConfigurationResponse
newDeleteSecurityConfigurationResponse pHttpStatus_ =
  DeleteSecurityConfigurationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteSecurityConfigurationResponse_httpStatus :: Lens.Lens' DeleteSecurityConfigurationResponse Prelude.Int
deleteSecurityConfigurationResponse_httpStatus = Lens.lens (\DeleteSecurityConfigurationResponse' {httpStatus} -> httpStatus) (\s@DeleteSecurityConfigurationResponse' {} a -> s {httpStatus = a} :: DeleteSecurityConfigurationResponse)

instance
  Prelude.NFData
    DeleteSecurityConfigurationResponse
  where
  rnf DeleteSecurityConfigurationResponse' {..} =
    Prelude.rnf httpStatus
