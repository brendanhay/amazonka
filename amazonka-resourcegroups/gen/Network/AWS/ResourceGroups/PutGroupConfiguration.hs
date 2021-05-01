{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.ResourceGroups.PutGroupConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches a service configuration to the specified group. This occurs
-- asynchronously, and can take time to complete. You can use
-- GetGroupConfiguration to check the status of the update.
--
-- __Minimum permissions__
--
-- To run this command, you must have the following permissions:
--
-- -   @resource-groups:PutGroupConfiguration@
module Network.AWS.ResourceGroups.PutGroupConfiguration
  ( -- * Creating a Request
    PutGroupConfiguration (..),
    newPutGroupConfiguration,

    -- * Request Lenses
    putGroupConfiguration_configuration,
    putGroupConfiguration_group,

    -- * Destructuring the Response
    PutGroupConfigurationResponse (..),
    newPutGroupConfigurationResponse,

    -- * Response Lenses
    putGroupConfigurationResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import Network.AWS.ResourceGroups.Types
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutGroupConfiguration' smart constructor.
data PutGroupConfiguration = PutGroupConfiguration'
  { -- | The new configuration to associate with the specified group. A
    -- configuration associates the resource group with an AWS service and
    -- specifies how the service can interact with the resources in the group.
    -- A configuration is an array of GroupConfigurationItem elements.
    --
    -- For information about the syntax of a service configuration, see
    -- <https://docs.aws.amazon.com/ARG/latest/APIReference/about-slg.html Service configurations for resource groups>.
    --
    -- A resource group can contain either a @Configuration@ or a
    -- @ResourceQuery@, but not both.
    configuration :: Prelude.Maybe [GroupConfigurationItem],
    -- | The name or ARN of the resource group with the configuration that you
    -- want to update.
    group' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutGroupConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configuration', 'putGroupConfiguration_configuration' - The new configuration to associate with the specified group. A
-- configuration associates the resource group with an AWS service and
-- specifies how the service can interact with the resources in the group.
-- A configuration is an array of GroupConfigurationItem elements.
--
-- For information about the syntax of a service configuration, see
-- <https://docs.aws.amazon.com/ARG/latest/APIReference/about-slg.html Service configurations for resource groups>.
--
-- A resource group can contain either a @Configuration@ or a
-- @ResourceQuery@, but not both.
--
-- 'group'', 'putGroupConfiguration_group' - The name or ARN of the resource group with the configuration that you
-- want to update.
newPutGroupConfiguration ::
  PutGroupConfiguration
newPutGroupConfiguration =
  PutGroupConfiguration'
    { configuration =
        Prelude.Nothing,
      group' = Prelude.Nothing
    }

-- | The new configuration to associate with the specified group. A
-- configuration associates the resource group with an AWS service and
-- specifies how the service can interact with the resources in the group.
-- A configuration is an array of GroupConfigurationItem elements.
--
-- For information about the syntax of a service configuration, see
-- <https://docs.aws.amazon.com/ARG/latest/APIReference/about-slg.html Service configurations for resource groups>.
--
-- A resource group can contain either a @Configuration@ or a
-- @ResourceQuery@, but not both.
putGroupConfiguration_configuration :: Lens.Lens' PutGroupConfiguration (Prelude.Maybe [GroupConfigurationItem])
putGroupConfiguration_configuration = Lens.lens (\PutGroupConfiguration' {configuration} -> configuration) (\s@PutGroupConfiguration' {} a -> s {configuration = a} :: PutGroupConfiguration) Prelude.. Lens.mapping Prelude._Coerce

-- | The name or ARN of the resource group with the configuration that you
-- want to update.
putGroupConfiguration_group :: Lens.Lens' PutGroupConfiguration (Prelude.Maybe Prelude.Text)
putGroupConfiguration_group = Lens.lens (\PutGroupConfiguration' {group'} -> group') (\s@PutGroupConfiguration' {} a -> s {group' = a} :: PutGroupConfiguration)

instance Prelude.AWSRequest PutGroupConfiguration where
  type
    Rs PutGroupConfiguration =
      PutGroupConfigurationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutGroupConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutGroupConfiguration

instance Prelude.NFData PutGroupConfiguration

instance Prelude.ToHeaders PutGroupConfiguration where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON PutGroupConfiguration where
  toJSON PutGroupConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Configuration" Prelude..=)
              Prelude.<$> configuration,
            ("Group" Prelude..=) Prelude.<$> group'
          ]
      )

instance Prelude.ToPath PutGroupConfiguration where
  toPath = Prelude.const "/put-group-configuration"

instance Prelude.ToQuery PutGroupConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutGroupConfigurationResponse' smart constructor.
data PutGroupConfigurationResponse = PutGroupConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutGroupConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putGroupConfigurationResponse_httpStatus' - The response's http status code.
newPutGroupConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutGroupConfigurationResponse
newPutGroupConfigurationResponse pHttpStatus_ =
  PutGroupConfigurationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
putGroupConfigurationResponse_httpStatus :: Lens.Lens' PutGroupConfigurationResponse Prelude.Int
putGroupConfigurationResponse_httpStatus = Lens.lens (\PutGroupConfigurationResponse' {httpStatus} -> httpStatus) (\s@PutGroupConfigurationResponse' {} a -> s {httpStatus = a} :: PutGroupConfigurationResponse)

instance Prelude.NFData PutGroupConfigurationResponse
