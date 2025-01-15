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
-- Module      : Amazonka.Config.DeleteRemediationConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the remediation configuration.
module Amazonka.Config.DeleteRemediationConfiguration
  ( -- * Creating a Request
    DeleteRemediationConfiguration (..),
    newDeleteRemediationConfiguration,

    -- * Request Lenses
    deleteRemediationConfiguration_resourceType,
    deleteRemediationConfiguration_configRuleName,

    -- * Destructuring the Response
    DeleteRemediationConfigurationResponse (..),
    newDeleteRemediationConfigurationResponse,

    -- * Response Lenses
    deleteRemediationConfigurationResponse_httpStatus,
  )
where

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteRemediationConfiguration' smart constructor.
data DeleteRemediationConfiguration = DeleteRemediationConfiguration'
  { -- | The type of a resource.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | The name of the Config rule for which you want to delete remediation
    -- configuration.
    configRuleName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRemediationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'deleteRemediationConfiguration_resourceType' - The type of a resource.
--
-- 'configRuleName', 'deleteRemediationConfiguration_configRuleName' - The name of the Config rule for which you want to delete remediation
-- configuration.
newDeleteRemediationConfiguration ::
  -- | 'configRuleName'
  Prelude.Text ->
  DeleteRemediationConfiguration
newDeleteRemediationConfiguration pConfigRuleName_ =
  DeleteRemediationConfiguration'
    { resourceType =
        Prelude.Nothing,
      configRuleName = pConfigRuleName_
    }

-- | The type of a resource.
deleteRemediationConfiguration_resourceType :: Lens.Lens' DeleteRemediationConfiguration (Prelude.Maybe Prelude.Text)
deleteRemediationConfiguration_resourceType = Lens.lens (\DeleteRemediationConfiguration' {resourceType} -> resourceType) (\s@DeleteRemediationConfiguration' {} a -> s {resourceType = a} :: DeleteRemediationConfiguration)

-- | The name of the Config rule for which you want to delete remediation
-- configuration.
deleteRemediationConfiguration_configRuleName :: Lens.Lens' DeleteRemediationConfiguration Prelude.Text
deleteRemediationConfiguration_configRuleName = Lens.lens (\DeleteRemediationConfiguration' {configRuleName} -> configRuleName) (\s@DeleteRemediationConfiguration' {} a -> s {configRuleName = a} :: DeleteRemediationConfiguration)

instance
  Core.AWSRequest
    DeleteRemediationConfiguration
  where
  type
    AWSResponse DeleteRemediationConfiguration =
      DeleteRemediationConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteRemediationConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteRemediationConfiguration
  where
  hashWithSalt
    _salt
    DeleteRemediationConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` resourceType
        `Prelude.hashWithSalt` configRuleName

instance
  Prelude.NFData
    DeleteRemediationConfiguration
  where
  rnf DeleteRemediationConfiguration' {..} =
    Prelude.rnf resourceType `Prelude.seq`
      Prelude.rnf configRuleName

instance
  Data.ToHeaders
    DeleteRemediationConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StarlingDoveService.DeleteRemediationConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteRemediationConfiguration where
  toJSON DeleteRemediationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ResourceType" Data..=) Prelude.<$> resourceType,
            Prelude.Just
              ("ConfigRuleName" Data..= configRuleName)
          ]
      )

instance Data.ToPath DeleteRemediationConfiguration where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteRemediationConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteRemediationConfigurationResponse' smart constructor.
data DeleteRemediationConfigurationResponse = DeleteRemediationConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRemediationConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteRemediationConfigurationResponse_httpStatus' - The response's http status code.
newDeleteRemediationConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteRemediationConfigurationResponse
newDeleteRemediationConfigurationResponse
  pHttpStatus_ =
    DeleteRemediationConfigurationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
deleteRemediationConfigurationResponse_httpStatus :: Lens.Lens' DeleteRemediationConfigurationResponse Prelude.Int
deleteRemediationConfigurationResponse_httpStatus = Lens.lens (\DeleteRemediationConfigurationResponse' {httpStatus} -> httpStatus) (\s@DeleteRemediationConfigurationResponse' {} a -> s {httpStatus = a} :: DeleteRemediationConfigurationResponse)

instance
  Prelude.NFData
    DeleteRemediationConfigurationResponse
  where
  rnf DeleteRemediationConfigurationResponse' {..} =
    Prelude.rnf httpStatus
