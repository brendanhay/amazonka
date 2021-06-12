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
-- Module      : Network.AWS.Config.DeleteRemediationConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the remediation configuration.
module Network.AWS.Config.DeleteRemediationConfiguration
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

import Network.AWS.Config.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteRemediationConfiguration' smart constructor.
data DeleteRemediationConfiguration = DeleteRemediationConfiguration'
  { -- | The type of a resource.
    resourceType :: Core.Maybe Core.Text,
    -- | The name of the AWS Config rule for which you want to delete remediation
    -- configuration.
    configRuleName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'configRuleName', 'deleteRemediationConfiguration_configRuleName' - The name of the AWS Config rule for which you want to delete remediation
-- configuration.
newDeleteRemediationConfiguration ::
  -- | 'configRuleName'
  Core.Text ->
  DeleteRemediationConfiguration
newDeleteRemediationConfiguration pConfigRuleName_ =
  DeleteRemediationConfiguration'
    { resourceType =
        Core.Nothing,
      configRuleName = pConfigRuleName_
    }

-- | The type of a resource.
deleteRemediationConfiguration_resourceType :: Lens.Lens' DeleteRemediationConfiguration (Core.Maybe Core.Text)
deleteRemediationConfiguration_resourceType = Lens.lens (\DeleteRemediationConfiguration' {resourceType} -> resourceType) (\s@DeleteRemediationConfiguration' {} a -> s {resourceType = a} :: DeleteRemediationConfiguration)

-- | The name of the AWS Config rule for which you want to delete remediation
-- configuration.
deleteRemediationConfiguration_configRuleName :: Lens.Lens' DeleteRemediationConfiguration Core.Text
deleteRemediationConfiguration_configRuleName = Lens.lens (\DeleteRemediationConfiguration' {configRuleName} -> configRuleName) (\s@DeleteRemediationConfiguration' {} a -> s {configRuleName = a} :: DeleteRemediationConfiguration)

instance
  Core.AWSRequest
    DeleteRemediationConfiguration
  where
  type
    AWSResponse DeleteRemediationConfiguration =
      DeleteRemediationConfigurationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteRemediationConfigurationResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteRemediationConfiguration

instance Core.NFData DeleteRemediationConfiguration

instance
  Core.ToHeaders
    DeleteRemediationConfiguration
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.DeleteRemediationConfiguration" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteRemediationConfiguration where
  toJSON DeleteRemediationConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ResourceType" Core..=) Core.<$> resourceType,
            Core.Just ("ConfigRuleName" Core..= configRuleName)
          ]
      )

instance Core.ToPath DeleteRemediationConfiguration where
  toPath = Core.const "/"

instance Core.ToQuery DeleteRemediationConfiguration where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteRemediationConfigurationResponse' smart constructor.
data DeleteRemediationConfigurationResponse = DeleteRemediationConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DeleteRemediationConfigurationResponse
newDeleteRemediationConfigurationResponse
  pHttpStatus_ =
    DeleteRemediationConfigurationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
deleteRemediationConfigurationResponse_httpStatus :: Lens.Lens' DeleteRemediationConfigurationResponse Core.Int
deleteRemediationConfigurationResponse_httpStatus = Lens.lens (\DeleteRemediationConfigurationResponse' {httpStatus} -> httpStatus) (\s@DeleteRemediationConfigurationResponse' {} a -> s {httpStatus = a} :: DeleteRemediationConfigurationResponse)

instance
  Core.NFData
    DeleteRemediationConfigurationResponse
