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
-- Module      : Amazonka.IoTWireless.DeleteNetworkAnalyzerConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a network analyzer configuration.
module Amazonka.IoTWireless.DeleteNetworkAnalyzerConfiguration
  ( -- * Creating a Request
    DeleteNetworkAnalyzerConfiguration (..),
    newDeleteNetworkAnalyzerConfiguration,

    -- * Request Lenses
    deleteNetworkAnalyzerConfiguration_configurationName,

    -- * Destructuring the Response
    DeleteNetworkAnalyzerConfigurationResponse (..),
    newDeleteNetworkAnalyzerConfigurationResponse,

    -- * Response Lenses
    deleteNetworkAnalyzerConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteNetworkAnalyzerConfiguration' smart constructor.
data DeleteNetworkAnalyzerConfiguration = DeleteNetworkAnalyzerConfiguration'
  { configurationName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteNetworkAnalyzerConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationName', 'deleteNetworkAnalyzerConfiguration_configurationName' - Undocumented member.
newDeleteNetworkAnalyzerConfiguration ::
  -- | 'configurationName'
  Prelude.Text ->
  DeleteNetworkAnalyzerConfiguration
newDeleteNetworkAnalyzerConfiguration
  pConfigurationName_ =
    DeleteNetworkAnalyzerConfiguration'
      { configurationName =
          pConfigurationName_
      }

-- | Undocumented member.
deleteNetworkAnalyzerConfiguration_configurationName :: Lens.Lens' DeleteNetworkAnalyzerConfiguration Prelude.Text
deleteNetworkAnalyzerConfiguration_configurationName = Lens.lens (\DeleteNetworkAnalyzerConfiguration' {configurationName} -> configurationName) (\s@DeleteNetworkAnalyzerConfiguration' {} a -> s {configurationName = a} :: DeleteNetworkAnalyzerConfiguration)

instance
  Core.AWSRequest
    DeleteNetworkAnalyzerConfiguration
  where
  type
    AWSResponse DeleteNetworkAnalyzerConfiguration =
      DeleteNetworkAnalyzerConfigurationResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteNetworkAnalyzerConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteNetworkAnalyzerConfiguration
  where
  hashWithSalt
    _salt
    DeleteNetworkAnalyzerConfiguration' {..} =
      _salt `Prelude.hashWithSalt` configurationName

instance
  Prelude.NFData
    DeleteNetworkAnalyzerConfiguration
  where
  rnf DeleteNetworkAnalyzerConfiguration' {..} =
    Prelude.rnf configurationName

instance
  Core.ToHeaders
    DeleteNetworkAnalyzerConfiguration
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    DeleteNetworkAnalyzerConfiguration
  where
  toPath DeleteNetworkAnalyzerConfiguration' {..} =
    Prelude.mconcat
      [ "/network-analyzer-configurations/",
        Core.toBS configurationName
      ]

instance
  Core.ToQuery
    DeleteNetworkAnalyzerConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteNetworkAnalyzerConfigurationResponse' smart constructor.
data DeleteNetworkAnalyzerConfigurationResponse = DeleteNetworkAnalyzerConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteNetworkAnalyzerConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteNetworkAnalyzerConfigurationResponse_httpStatus' - The response's http status code.
newDeleteNetworkAnalyzerConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteNetworkAnalyzerConfigurationResponse
newDeleteNetworkAnalyzerConfigurationResponse
  pHttpStatus_ =
    DeleteNetworkAnalyzerConfigurationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
deleteNetworkAnalyzerConfigurationResponse_httpStatus :: Lens.Lens' DeleteNetworkAnalyzerConfigurationResponse Prelude.Int
deleteNetworkAnalyzerConfigurationResponse_httpStatus = Lens.lens (\DeleteNetworkAnalyzerConfigurationResponse' {httpStatus} -> httpStatus) (\s@DeleteNetworkAnalyzerConfigurationResponse' {} a -> s {httpStatus = a} :: DeleteNetworkAnalyzerConfigurationResponse)

instance
  Prelude.NFData
    DeleteNetworkAnalyzerConfigurationResponse
  where
  rnf DeleteNetworkAnalyzerConfigurationResponse' {..} =
    Prelude.rnf httpStatus
