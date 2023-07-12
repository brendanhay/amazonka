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
-- Module      : Amazonka.IoT.DeleteDomainConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified domain configuration.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions DeleteDomainConfiguration>
-- action.
module Amazonka.IoT.DeleteDomainConfiguration
  ( -- * Creating a Request
    DeleteDomainConfiguration (..),
    newDeleteDomainConfiguration,

    -- * Request Lenses
    deleteDomainConfiguration_domainConfigurationName,

    -- * Destructuring the Response
    DeleteDomainConfigurationResponse (..),
    newDeleteDomainConfigurationResponse,

    -- * Response Lenses
    deleteDomainConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteDomainConfiguration' smart constructor.
data DeleteDomainConfiguration = DeleteDomainConfiguration'
  { -- | The name of the domain configuration to be deleted.
    domainConfigurationName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDomainConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainConfigurationName', 'deleteDomainConfiguration_domainConfigurationName' - The name of the domain configuration to be deleted.
newDeleteDomainConfiguration ::
  -- | 'domainConfigurationName'
  Prelude.Text ->
  DeleteDomainConfiguration
newDeleteDomainConfiguration
  pDomainConfigurationName_ =
    DeleteDomainConfiguration'
      { domainConfigurationName =
          pDomainConfigurationName_
      }

-- | The name of the domain configuration to be deleted.
deleteDomainConfiguration_domainConfigurationName :: Lens.Lens' DeleteDomainConfiguration Prelude.Text
deleteDomainConfiguration_domainConfigurationName = Lens.lens (\DeleteDomainConfiguration' {domainConfigurationName} -> domainConfigurationName) (\s@DeleteDomainConfiguration' {} a -> s {domainConfigurationName = a} :: DeleteDomainConfiguration)

instance Core.AWSRequest DeleteDomainConfiguration where
  type
    AWSResponse DeleteDomainConfiguration =
      DeleteDomainConfigurationResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteDomainConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteDomainConfiguration where
  hashWithSalt _salt DeleteDomainConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` domainConfigurationName

instance Prelude.NFData DeleteDomainConfiguration where
  rnf DeleteDomainConfiguration' {..} =
    Prelude.rnf domainConfigurationName

instance Data.ToHeaders DeleteDomainConfiguration where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteDomainConfiguration where
  toPath DeleteDomainConfiguration' {..} =
    Prelude.mconcat
      [ "/domainConfigurations/",
        Data.toBS domainConfigurationName
      ]

instance Data.ToQuery DeleteDomainConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDomainConfigurationResponse' smart constructor.
data DeleteDomainConfigurationResponse = DeleteDomainConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDomainConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteDomainConfigurationResponse_httpStatus' - The response's http status code.
newDeleteDomainConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteDomainConfigurationResponse
newDeleteDomainConfigurationResponse pHttpStatus_ =
  DeleteDomainConfigurationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteDomainConfigurationResponse_httpStatus :: Lens.Lens' DeleteDomainConfigurationResponse Prelude.Int
deleteDomainConfigurationResponse_httpStatus = Lens.lens (\DeleteDomainConfigurationResponse' {httpStatus} -> httpStatus) (\s@DeleteDomainConfigurationResponse' {} a -> s {httpStatus = a} :: DeleteDomainConfigurationResponse)

instance
  Prelude.NFData
    DeleteDomainConfigurationResponse
  where
  rnf DeleteDomainConfigurationResponse' {..} =
    Prelude.rnf httpStatus
