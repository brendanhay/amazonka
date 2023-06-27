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
-- Module      : Amazonka.ResourceGroups.PutGroupConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.ResourceGroups.PutGroupConfiguration
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResourceGroups.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutGroupConfiguration' smart constructor.
data PutGroupConfiguration = PutGroupConfiguration'
  { -- | The new configuration to associate with the specified group. A
    -- configuration associates the resource group with an Amazon Web Services
    -- service and specifies how the service can interact with the resources in
    -- the group. A configuration is an array of GroupConfigurationItem
    -- elements.
    --
    -- For information about the syntax of a service configuration, see
    -- <https://docs.aws.amazon.com/ARG/latest/APIReference/about-slg.html Service configurations for Resource Groups>.
    --
    -- A resource group can contain either a @Configuration@ or a
    -- @ResourceQuery@, but not both.
    configuration :: Prelude.Maybe [GroupConfigurationItem],
    -- | The name or ARN of the resource group with the configuration that you
    -- want to update.
    group' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutGroupConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configuration', 'putGroupConfiguration_configuration' - The new configuration to associate with the specified group. A
-- configuration associates the resource group with an Amazon Web Services
-- service and specifies how the service can interact with the resources in
-- the group. A configuration is an array of GroupConfigurationItem
-- elements.
--
-- For information about the syntax of a service configuration, see
-- <https://docs.aws.amazon.com/ARG/latest/APIReference/about-slg.html Service configurations for Resource Groups>.
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
-- configuration associates the resource group with an Amazon Web Services
-- service and specifies how the service can interact with the resources in
-- the group. A configuration is an array of GroupConfigurationItem
-- elements.
--
-- For information about the syntax of a service configuration, see
-- <https://docs.aws.amazon.com/ARG/latest/APIReference/about-slg.html Service configurations for Resource Groups>.
--
-- A resource group can contain either a @Configuration@ or a
-- @ResourceQuery@, but not both.
putGroupConfiguration_configuration :: Lens.Lens' PutGroupConfiguration (Prelude.Maybe [GroupConfigurationItem])
putGroupConfiguration_configuration = Lens.lens (\PutGroupConfiguration' {configuration} -> configuration) (\s@PutGroupConfiguration' {} a -> s {configuration = a} :: PutGroupConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The name or ARN of the resource group with the configuration that you
-- want to update.
putGroupConfiguration_group :: Lens.Lens' PutGroupConfiguration (Prelude.Maybe Prelude.Text)
putGroupConfiguration_group = Lens.lens (\PutGroupConfiguration' {group'} -> group') (\s@PutGroupConfiguration' {} a -> s {group' = a} :: PutGroupConfiguration)

instance Core.AWSRequest PutGroupConfiguration where
  type
    AWSResponse PutGroupConfiguration =
      PutGroupConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutGroupConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutGroupConfiguration where
  hashWithSalt _salt PutGroupConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` configuration
      `Prelude.hashWithSalt` group'

instance Prelude.NFData PutGroupConfiguration where
  rnf PutGroupConfiguration' {..} =
    Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf group'

instance Data.ToHeaders PutGroupConfiguration where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON PutGroupConfiguration where
  toJSON PutGroupConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Configuration" Data..=) Prelude.<$> configuration,
            ("Group" Data..=) Prelude.<$> group'
          ]
      )

instance Data.ToPath PutGroupConfiguration where
  toPath = Prelude.const "/put-group-configuration"

instance Data.ToQuery PutGroupConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutGroupConfigurationResponse' smart constructor.
data PutGroupConfigurationResponse = PutGroupConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData PutGroupConfigurationResponse where
  rnf PutGroupConfigurationResponse' {..} =
    Prelude.rnf httpStatus
