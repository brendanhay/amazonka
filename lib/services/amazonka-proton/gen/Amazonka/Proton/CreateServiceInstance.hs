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
-- Module      : Amazonka.Proton.CreateServiceInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a service instance.
module Amazonka.Proton.CreateServiceInstance
  ( -- * Creating a Request
    CreateServiceInstance (..),
    newCreateServiceInstance,

    -- * Request Lenses
    createServiceInstance_clientToken,
    createServiceInstance_tags,
    createServiceInstance_templateMajorVersion,
    createServiceInstance_templateMinorVersion,
    createServiceInstance_name,
    createServiceInstance_serviceName,
    createServiceInstance_spec,

    -- * Destructuring the Response
    CreateServiceInstanceResponse (..),
    newCreateServiceInstanceResponse,

    -- * Response Lenses
    createServiceInstanceResponse_httpStatus,
    createServiceInstanceResponse_serviceInstance,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateServiceInstance' smart constructor.
data CreateServiceInstance = CreateServiceInstance'
  { -- | The client token of the service instance to create.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | An optional list of metadata items that you can associate with the
    -- Proton service instance. A tag is a key-value pair.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/proton/latest/userguide/resources.html Proton resources and tagging>
    -- in the /Proton User Guide/.
    tags :: Prelude.Maybe [Tag],
    -- | To create a new major and minor version of the service template,
    -- /exclude/ @major Version@.
    templateMajorVersion :: Prelude.Maybe Prelude.Text,
    -- | To create a new minor version of the service template, include a
    -- @major Version@.
    templateMinorVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the service instance to create.
    name :: Prelude.Text,
    -- | The name of the service the service instance is added to.
    serviceName :: Prelude.Text,
    -- | The spec for the service instance you want to create.
    spec :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateServiceInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createServiceInstance_clientToken' - The client token of the service instance to create.
--
-- 'tags', 'createServiceInstance_tags' - An optional list of metadata items that you can associate with the
-- Proton service instance. A tag is a key-value pair.
--
-- For more information, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/resources.html Proton resources and tagging>
-- in the /Proton User Guide/.
--
-- 'templateMajorVersion', 'createServiceInstance_templateMajorVersion' - To create a new major and minor version of the service template,
-- /exclude/ @major Version@.
--
-- 'templateMinorVersion', 'createServiceInstance_templateMinorVersion' - To create a new minor version of the service template, include a
-- @major Version@.
--
-- 'name', 'createServiceInstance_name' - The name of the service instance to create.
--
-- 'serviceName', 'createServiceInstance_serviceName' - The name of the service the service instance is added to.
--
-- 'spec', 'createServiceInstance_spec' - The spec for the service instance you want to create.
newCreateServiceInstance ::
  -- | 'name'
  Prelude.Text ->
  -- | 'serviceName'
  Prelude.Text ->
  -- | 'spec'
  Prelude.Text ->
  CreateServiceInstance
newCreateServiceInstance pName_ pServiceName_ pSpec_ =
  CreateServiceInstance'
    { clientToken =
        Prelude.Nothing,
      tags = Prelude.Nothing,
      templateMajorVersion = Prelude.Nothing,
      templateMinorVersion = Prelude.Nothing,
      name = pName_,
      serviceName = pServiceName_,
      spec = Data._Sensitive Lens.# pSpec_
    }

-- | The client token of the service instance to create.
createServiceInstance_clientToken :: Lens.Lens' CreateServiceInstance (Prelude.Maybe Prelude.Text)
createServiceInstance_clientToken = Lens.lens (\CreateServiceInstance' {clientToken} -> clientToken) (\s@CreateServiceInstance' {} a -> s {clientToken = a} :: CreateServiceInstance)

-- | An optional list of metadata items that you can associate with the
-- Proton service instance. A tag is a key-value pair.
--
-- For more information, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/resources.html Proton resources and tagging>
-- in the /Proton User Guide/.
createServiceInstance_tags :: Lens.Lens' CreateServiceInstance (Prelude.Maybe [Tag])
createServiceInstance_tags = Lens.lens (\CreateServiceInstance' {tags} -> tags) (\s@CreateServiceInstance' {} a -> s {tags = a} :: CreateServiceInstance) Prelude.. Lens.mapping Lens.coerced

-- | To create a new major and minor version of the service template,
-- /exclude/ @major Version@.
createServiceInstance_templateMajorVersion :: Lens.Lens' CreateServiceInstance (Prelude.Maybe Prelude.Text)
createServiceInstance_templateMajorVersion = Lens.lens (\CreateServiceInstance' {templateMajorVersion} -> templateMajorVersion) (\s@CreateServiceInstance' {} a -> s {templateMajorVersion = a} :: CreateServiceInstance)

-- | To create a new minor version of the service template, include a
-- @major Version@.
createServiceInstance_templateMinorVersion :: Lens.Lens' CreateServiceInstance (Prelude.Maybe Prelude.Text)
createServiceInstance_templateMinorVersion = Lens.lens (\CreateServiceInstance' {templateMinorVersion} -> templateMinorVersion) (\s@CreateServiceInstance' {} a -> s {templateMinorVersion = a} :: CreateServiceInstance)

-- | The name of the service instance to create.
createServiceInstance_name :: Lens.Lens' CreateServiceInstance Prelude.Text
createServiceInstance_name = Lens.lens (\CreateServiceInstance' {name} -> name) (\s@CreateServiceInstance' {} a -> s {name = a} :: CreateServiceInstance)

-- | The name of the service the service instance is added to.
createServiceInstance_serviceName :: Lens.Lens' CreateServiceInstance Prelude.Text
createServiceInstance_serviceName = Lens.lens (\CreateServiceInstance' {serviceName} -> serviceName) (\s@CreateServiceInstance' {} a -> s {serviceName = a} :: CreateServiceInstance)

-- | The spec for the service instance you want to create.
createServiceInstance_spec :: Lens.Lens' CreateServiceInstance Prelude.Text
createServiceInstance_spec = Lens.lens (\CreateServiceInstance' {spec} -> spec) (\s@CreateServiceInstance' {} a -> s {spec = a} :: CreateServiceInstance) Prelude.. Data._Sensitive

instance Core.AWSRequest CreateServiceInstance where
  type
    AWSResponse CreateServiceInstance =
      CreateServiceInstanceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateServiceInstanceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "serviceInstance")
      )

instance Prelude.Hashable CreateServiceInstance where
  hashWithSalt _salt CreateServiceInstance' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` templateMajorVersion
      `Prelude.hashWithSalt` templateMinorVersion
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` serviceName
      `Prelude.hashWithSalt` spec

instance Prelude.NFData CreateServiceInstance where
  rnf CreateServiceInstance' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf templateMajorVersion
      `Prelude.seq` Prelude.rnf templateMinorVersion
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf serviceName
      `Prelude.seq` Prelude.rnf spec

instance Data.ToHeaders CreateServiceInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AwsProton20200720.CreateServiceInstance" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateServiceInstance where
  toJSON CreateServiceInstance' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("tags" Data..=) Prelude.<$> tags,
            ("templateMajorVersion" Data..=)
              Prelude.<$> templateMajorVersion,
            ("templateMinorVersion" Data..=)
              Prelude.<$> templateMinorVersion,
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("serviceName" Data..= serviceName),
            Prelude.Just ("spec" Data..= spec)
          ]
      )

instance Data.ToPath CreateServiceInstance where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateServiceInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateServiceInstanceResponse' smart constructor.
data CreateServiceInstanceResponse = CreateServiceInstanceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The detailed data of the service instance being created.
    serviceInstance :: ServiceInstance
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateServiceInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createServiceInstanceResponse_httpStatus' - The response's http status code.
--
-- 'serviceInstance', 'createServiceInstanceResponse_serviceInstance' - The detailed data of the service instance being created.
newCreateServiceInstanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'serviceInstance'
  ServiceInstance ->
  CreateServiceInstanceResponse
newCreateServiceInstanceResponse
  pHttpStatus_
  pServiceInstance_ =
    CreateServiceInstanceResponse'
      { httpStatus =
          pHttpStatus_,
        serviceInstance = pServiceInstance_
      }

-- | The response's http status code.
createServiceInstanceResponse_httpStatus :: Lens.Lens' CreateServiceInstanceResponse Prelude.Int
createServiceInstanceResponse_httpStatus = Lens.lens (\CreateServiceInstanceResponse' {httpStatus} -> httpStatus) (\s@CreateServiceInstanceResponse' {} a -> s {httpStatus = a} :: CreateServiceInstanceResponse)

-- | The detailed data of the service instance being created.
createServiceInstanceResponse_serviceInstance :: Lens.Lens' CreateServiceInstanceResponse ServiceInstance
createServiceInstanceResponse_serviceInstance = Lens.lens (\CreateServiceInstanceResponse' {serviceInstance} -> serviceInstance) (\s@CreateServiceInstanceResponse' {} a -> s {serviceInstance = a} :: CreateServiceInstanceResponse)

instance Prelude.NFData CreateServiceInstanceResponse where
  rnf CreateServiceInstanceResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf serviceInstance
