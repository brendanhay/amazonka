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
-- Module      : Amazonka.Connect.UpdateContactFlowModuleMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates metadata about specified flow module.
module Amazonka.Connect.UpdateContactFlowModuleMetadata
  ( -- * Creating a Request
    UpdateContactFlowModuleMetadata (..),
    newUpdateContactFlowModuleMetadata,

    -- * Request Lenses
    updateContactFlowModuleMetadata_name,
    updateContactFlowModuleMetadata_state,
    updateContactFlowModuleMetadata_description,
    updateContactFlowModuleMetadata_instanceId,
    updateContactFlowModuleMetadata_contactFlowModuleId,

    -- * Destructuring the Response
    UpdateContactFlowModuleMetadataResponse (..),
    newUpdateContactFlowModuleMetadataResponse,

    -- * Response Lenses
    updateContactFlowModuleMetadataResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateContactFlowModuleMetadata' smart constructor.
data UpdateContactFlowModuleMetadata = UpdateContactFlowModuleMetadata'
  { -- | The name of the flow module.
    name :: Prelude.Maybe Prelude.Text,
    -- | The state of flow module.
    state :: Prelude.Maybe ContactFlowModuleState,
    -- | The description of the flow module.
    description :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | The identifier of the flow module.
    contactFlowModuleId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateContactFlowModuleMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateContactFlowModuleMetadata_name' - The name of the flow module.
--
-- 'state', 'updateContactFlowModuleMetadata_state' - The state of flow module.
--
-- 'description', 'updateContactFlowModuleMetadata_description' - The description of the flow module.
--
-- 'instanceId', 'updateContactFlowModuleMetadata_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'contactFlowModuleId', 'updateContactFlowModuleMetadata_contactFlowModuleId' - The identifier of the flow module.
newUpdateContactFlowModuleMetadata ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'contactFlowModuleId'
  Prelude.Text ->
  UpdateContactFlowModuleMetadata
newUpdateContactFlowModuleMetadata
  pInstanceId_
  pContactFlowModuleId_ =
    UpdateContactFlowModuleMetadata'
      { name =
          Prelude.Nothing,
        state = Prelude.Nothing,
        description = Prelude.Nothing,
        instanceId = pInstanceId_,
        contactFlowModuleId =
          pContactFlowModuleId_
      }

-- | The name of the flow module.
updateContactFlowModuleMetadata_name :: Lens.Lens' UpdateContactFlowModuleMetadata (Prelude.Maybe Prelude.Text)
updateContactFlowModuleMetadata_name = Lens.lens (\UpdateContactFlowModuleMetadata' {name} -> name) (\s@UpdateContactFlowModuleMetadata' {} a -> s {name = a} :: UpdateContactFlowModuleMetadata)

-- | The state of flow module.
updateContactFlowModuleMetadata_state :: Lens.Lens' UpdateContactFlowModuleMetadata (Prelude.Maybe ContactFlowModuleState)
updateContactFlowModuleMetadata_state = Lens.lens (\UpdateContactFlowModuleMetadata' {state} -> state) (\s@UpdateContactFlowModuleMetadata' {} a -> s {state = a} :: UpdateContactFlowModuleMetadata)

-- | The description of the flow module.
updateContactFlowModuleMetadata_description :: Lens.Lens' UpdateContactFlowModuleMetadata (Prelude.Maybe Prelude.Text)
updateContactFlowModuleMetadata_description = Lens.lens (\UpdateContactFlowModuleMetadata' {description} -> description) (\s@UpdateContactFlowModuleMetadata' {} a -> s {description = a} :: UpdateContactFlowModuleMetadata)

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
updateContactFlowModuleMetadata_instanceId :: Lens.Lens' UpdateContactFlowModuleMetadata Prelude.Text
updateContactFlowModuleMetadata_instanceId = Lens.lens (\UpdateContactFlowModuleMetadata' {instanceId} -> instanceId) (\s@UpdateContactFlowModuleMetadata' {} a -> s {instanceId = a} :: UpdateContactFlowModuleMetadata)

-- | The identifier of the flow module.
updateContactFlowModuleMetadata_contactFlowModuleId :: Lens.Lens' UpdateContactFlowModuleMetadata Prelude.Text
updateContactFlowModuleMetadata_contactFlowModuleId = Lens.lens (\UpdateContactFlowModuleMetadata' {contactFlowModuleId} -> contactFlowModuleId) (\s@UpdateContactFlowModuleMetadata' {} a -> s {contactFlowModuleId = a} :: UpdateContactFlowModuleMetadata)

instance
  Core.AWSRequest
    UpdateContactFlowModuleMetadata
  where
  type
    AWSResponse UpdateContactFlowModuleMetadata =
      UpdateContactFlowModuleMetadataResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateContactFlowModuleMetadataResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateContactFlowModuleMetadata
  where
  hashWithSalt
    _salt
    UpdateContactFlowModuleMetadata' {..} =
      _salt `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` state
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` instanceId
        `Prelude.hashWithSalt` contactFlowModuleId

instance
  Prelude.NFData
    UpdateContactFlowModuleMetadata
  where
  rnf UpdateContactFlowModuleMetadata' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf contactFlowModuleId

instance
  Data.ToHeaders
    UpdateContactFlowModuleMetadata
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateContactFlowModuleMetadata where
  toJSON UpdateContactFlowModuleMetadata' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            ("State" Data..=) Prelude.<$> state,
            ("Description" Data..=) Prelude.<$> description
          ]
      )

instance Data.ToPath UpdateContactFlowModuleMetadata where
  toPath UpdateContactFlowModuleMetadata' {..} =
    Prelude.mconcat
      [ "/contact-flow-modules/",
        Data.toBS instanceId,
        "/",
        Data.toBS contactFlowModuleId,
        "/metadata"
      ]

instance Data.ToQuery UpdateContactFlowModuleMetadata where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateContactFlowModuleMetadataResponse' smart constructor.
data UpdateContactFlowModuleMetadataResponse = UpdateContactFlowModuleMetadataResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateContactFlowModuleMetadataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateContactFlowModuleMetadataResponse_httpStatus' - The response's http status code.
newUpdateContactFlowModuleMetadataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateContactFlowModuleMetadataResponse
newUpdateContactFlowModuleMetadataResponse
  pHttpStatus_ =
    UpdateContactFlowModuleMetadataResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
updateContactFlowModuleMetadataResponse_httpStatus :: Lens.Lens' UpdateContactFlowModuleMetadataResponse Prelude.Int
updateContactFlowModuleMetadataResponse_httpStatus = Lens.lens (\UpdateContactFlowModuleMetadataResponse' {httpStatus} -> httpStatus) (\s@UpdateContactFlowModuleMetadataResponse' {} a -> s {httpStatus = a} :: UpdateContactFlowModuleMetadataResponse)

instance
  Prelude.NFData
    UpdateContactFlowModuleMetadataResponse
  where
  rnf UpdateContactFlowModuleMetadataResponse' {..} =
    Prelude.rnf httpStatus
