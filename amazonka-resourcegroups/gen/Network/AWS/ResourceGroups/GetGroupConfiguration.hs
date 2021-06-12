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
-- Module      : Network.AWS.ResourceGroups.GetGroupConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the service configuration associated with the specified resource
-- group. For details about the service configuration syntax, see
-- <https://docs.aws.amazon.com/ARG/latest/APIReference/about-slg.html Service configurations for resource groups>.
--
-- __Minimum permissions__
--
-- To run this command, you must have the following permissions:
--
-- -   @resource-groups:GetGroupConfiguration@
module Network.AWS.ResourceGroups.GetGroupConfiguration
  ( -- * Creating a Request
    GetGroupConfiguration (..),
    newGetGroupConfiguration,

    -- * Request Lenses
    getGroupConfiguration_group,

    -- * Destructuring the Response
    GetGroupConfigurationResponse (..),
    newGetGroupConfigurationResponse,

    -- * Response Lenses
    getGroupConfigurationResponse_groupConfiguration,
    getGroupConfigurationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import Network.AWS.ResourceGroups.Types
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetGroupConfiguration' smart constructor.
data GetGroupConfiguration = GetGroupConfiguration'
  { -- | The name or the ARN of the resource group.
    group' :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetGroupConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'group'', 'getGroupConfiguration_group' - The name or the ARN of the resource group.
newGetGroupConfiguration ::
  GetGroupConfiguration
newGetGroupConfiguration =
  GetGroupConfiguration' {group' = Core.Nothing}

-- | The name or the ARN of the resource group.
getGroupConfiguration_group :: Lens.Lens' GetGroupConfiguration (Core.Maybe Core.Text)
getGroupConfiguration_group = Lens.lens (\GetGroupConfiguration' {group'} -> group') (\s@GetGroupConfiguration' {} a -> s {group' = a} :: GetGroupConfiguration)

instance Core.AWSRequest GetGroupConfiguration where
  type
    AWSResponse GetGroupConfiguration =
      GetGroupConfigurationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetGroupConfigurationResponse'
            Core.<$> (x Core..?> "GroupConfiguration")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetGroupConfiguration

instance Core.NFData GetGroupConfiguration

instance Core.ToHeaders GetGroupConfiguration where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON GetGroupConfiguration where
  toJSON GetGroupConfiguration' {..} =
    Core.object
      (Core.catMaybes [("Group" Core..=) Core.<$> group'])

instance Core.ToPath GetGroupConfiguration where
  toPath = Core.const "/get-group-configuration"

instance Core.ToQuery GetGroupConfiguration where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetGroupConfigurationResponse' smart constructor.
data GetGroupConfigurationResponse = GetGroupConfigurationResponse'
  { -- | The service configuration associated with the specified group. For
    -- details about the service configuration syntax, see
    -- <https://docs.aws.amazon.com/ARG/latest/APIReference/about-slg.html Service configurations for resource groups>.
    groupConfiguration :: Core.Maybe GroupConfiguration,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetGroupConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupConfiguration', 'getGroupConfigurationResponse_groupConfiguration' - The service configuration associated with the specified group. For
-- details about the service configuration syntax, see
-- <https://docs.aws.amazon.com/ARG/latest/APIReference/about-slg.html Service configurations for resource groups>.
--
-- 'httpStatus', 'getGroupConfigurationResponse_httpStatus' - The response's http status code.
newGetGroupConfigurationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetGroupConfigurationResponse
newGetGroupConfigurationResponse pHttpStatus_ =
  GetGroupConfigurationResponse'
    { groupConfiguration =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The service configuration associated with the specified group. For
-- details about the service configuration syntax, see
-- <https://docs.aws.amazon.com/ARG/latest/APIReference/about-slg.html Service configurations for resource groups>.
getGroupConfigurationResponse_groupConfiguration :: Lens.Lens' GetGroupConfigurationResponse (Core.Maybe GroupConfiguration)
getGroupConfigurationResponse_groupConfiguration = Lens.lens (\GetGroupConfigurationResponse' {groupConfiguration} -> groupConfiguration) (\s@GetGroupConfigurationResponse' {} a -> s {groupConfiguration = a} :: GetGroupConfigurationResponse)

-- | The response's http status code.
getGroupConfigurationResponse_httpStatus :: Lens.Lens' GetGroupConfigurationResponse Core.Int
getGroupConfigurationResponse_httpStatus = Lens.lens (\GetGroupConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetGroupConfigurationResponse' {} a -> s {httpStatus = a} :: GetGroupConfigurationResponse)

instance Core.NFData GetGroupConfigurationResponse
