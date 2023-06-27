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
-- Module      : Amazonka.TNB.UpdateSolNetworkInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update a network instance.
--
-- A network instance is a single network created in Amazon Web Services
-- TNB that can be deployed and on which life-cycle operations (like
-- terminate, update, and delete) can be performed.
module Amazonka.TNB.UpdateSolNetworkInstance
  ( -- * Creating a Request
    UpdateSolNetworkInstance (..),
    newUpdateSolNetworkInstance,

    -- * Request Lenses
    updateSolNetworkInstance_modifyVnfInfoData,
    updateSolNetworkInstance_tags,
    updateSolNetworkInstance_nsInstanceId,
    updateSolNetworkInstance_updateType,

    -- * Destructuring the Response
    UpdateSolNetworkInstanceResponse (..),
    newUpdateSolNetworkInstanceResponse,

    -- * Response Lenses
    updateSolNetworkInstanceResponse_nsLcmOpOccId,
    updateSolNetworkInstanceResponse_tags,
    updateSolNetworkInstanceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.TNB.Types

-- | /See:/ 'newUpdateSolNetworkInstance' smart constructor.
data UpdateSolNetworkInstance = UpdateSolNetworkInstance'
  { -- | Identifies the network function information parameters and\/or the
    -- configurable properties of the network function to be modified.
    modifyVnfInfoData :: Prelude.Maybe UpdateSolNetworkModify,
    -- | A tag is a label that you assign to an Amazon Web Services resource.
    -- Each tag consists of a key and an optional value. When you use this API,
    -- the tags are transferred to the network operation that is created. Use
    -- tags to search and filter your resources or track your Amazon Web
    -- Services costs.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | ID of the network instance.
    nsInstanceId :: Prelude.Text,
    -- | The type of update.
    updateType :: UpdateSolNetworkType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSolNetworkInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modifyVnfInfoData', 'updateSolNetworkInstance_modifyVnfInfoData' - Identifies the network function information parameters and\/or the
-- configurable properties of the network function to be modified.
--
-- 'tags', 'updateSolNetworkInstance_tags' - A tag is a label that you assign to an Amazon Web Services resource.
-- Each tag consists of a key and an optional value. When you use this API,
-- the tags are transferred to the network operation that is created. Use
-- tags to search and filter your resources or track your Amazon Web
-- Services costs.
--
-- 'nsInstanceId', 'updateSolNetworkInstance_nsInstanceId' - ID of the network instance.
--
-- 'updateType', 'updateSolNetworkInstance_updateType' - The type of update.
newUpdateSolNetworkInstance ::
  -- | 'nsInstanceId'
  Prelude.Text ->
  -- | 'updateType'
  UpdateSolNetworkType ->
  UpdateSolNetworkInstance
newUpdateSolNetworkInstance
  pNsInstanceId_
  pUpdateType_ =
    UpdateSolNetworkInstance'
      { modifyVnfInfoData =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        nsInstanceId = pNsInstanceId_,
        updateType = pUpdateType_
      }

-- | Identifies the network function information parameters and\/or the
-- configurable properties of the network function to be modified.
updateSolNetworkInstance_modifyVnfInfoData :: Lens.Lens' UpdateSolNetworkInstance (Prelude.Maybe UpdateSolNetworkModify)
updateSolNetworkInstance_modifyVnfInfoData = Lens.lens (\UpdateSolNetworkInstance' {modifyVnfInfoData} -> modifyVnfInfoData) (\s@UpdateSolNetworkInstance' {} a -> s {modifyVnfInfoData = a} :: UpdateSolNetworkInstance)

-- | A tag is a label that you assign to an Amazon Web Services resource.
-- Each tag consists of a key and an optional value. When you use this API,
-- the tags are transferred to the network operation that is created. Use
-- tags to search and filter your resources or track your Amazon Web
-- Services costs.
updateSolNetworkInstance_tags :: Lens.Lens' UpdateSolNetworkInstance (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateSolNetworkInstance_tags = Lens.lens (\UpdateSolNetworkInstance' {tags} -> tags) (\s@UpdateSolNetworkInstance' {} a -> s {tags = a} :: UpdateSolNetworkInstance) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | ID of the network instance.
updateSolNetworkInstance_nsInstanceId :: Lens.Lens' UpdateSolNetworkInstance Prelude.Text
updateSolNetworkInstance_nsInstanceId = Lens.lens (\UpdateSolNetworkInstance' {nsInstanceId} -> nsInstanceId) (\s@UpdateSolNetworkInstance' {} a -> s {nsInstanceId = a} :: UpdateSolNetworkInstance)

-- | The type of update.
updateSolNetworkInstance_updateType :: Lens.Lens' UpdateSolNetworkInstance UpdateSolNetworkType
updateSolNetworkInstance_updateType = Lens.lens (\UpdateSolNetworkInstance' {updateType} -> updateType) (\s@UpdateSolNetworkInstance' {} a -> s {updateType = a} :: UpdateSolNetworkInstance)

instance Core.AWSRequest UpdateSolNetworkInstance where
  type
    AWSResponse UpdateSolNetworkInstance =
      UpdateSolNetworkInstanceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSolNetworkInstanceResponse'
            Prelude.<$> (x Data..?> "nsLcmOpOccId")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateSolNetworkInstance where
  hashWithSalt _salt UpdateSolNetworkInstance' {..} =
    _salt
      `Prelude.hashWithSalt` modifyVnfInfoData
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` nsInstanceId
      `Prelude.hashWithSalt` updateType

instance Prelude.NFData UpdateSolNetworkInstance where
  rnf UpdateSolNetworkInstance' {..} =
    Prelude.rnf modifyVnfInfoData
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf nsInstanceId
      `Prelude.seq` Prelude.rnf updateType

instance Data.ToHeaders UpdateSolNetworkInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateSolNetworkInstance where
  toJSON UpdateSolNetworkInstance' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("modifyVnfInfoData" Data..=)
              Prelude.<$> modifyVnfInfoData,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("updateType" Data..= updateType)
          ]
      )

instance Data.ToPath UpdateSolNetworkInstance where
  toPath UpdateSolNetworkInstance' {..} =
    Prelude.mconcat
      [ "/sol/nslcm/v1/ns_instances/",
        Data.toBS nsInstanceId,
        "/update"
      ]

instance Data.ToQuery UpdateSolNetworkInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSolNetworkInstanceResponse' smart constructor.
data UpdateSolNetworkInstanceResponse = UpdateSolNetworkInstanceResponse'
  { -- | The identifier of the network operation.
    nsLcmOpOccId :: Prelude.Maybe Prelude.Text,
    -- | A tag is a label that you assign to an Amazon Web Services resource.
    -- Each tag consists of a key and an optional value. When you use this API,
    -- the tags are transferred to the network operation that is created. Use
    -- tags to search and filter your resources or track your Amazon Web
    -- Services costs.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSolNetworkInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nsLcmOpOccId', 'updateSolNetworkInstanceResponse_nsLcmOpOccId' - The identifier of the network operation.
--
-- 'tags', 'updateSolNetworkInstanceResponse_tags' - A tag is a label that you assign to an Amazon Web Services resource.
-- Each tag consists of a key and an optional value. When you use this API,
-- the tags are transferred to the network operation that is created. Use
-- tags to search and filter your resources or track your Amazon Web
-- Services costs.
--
-- 'httpStatus', 'updateSolNetworkInstanceResponse_httpStatus' - The response's http status code.
newUpdateSolNetworkInstanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateSolNetworkInstanceResponse
newUpdateSolNetworkInstanceResponse pHttpStatus_ =
  UpdateSolNetworkInstanceResponse'
    { nsLcmOpOccId =
        Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of the network operation.
updateSolNetworkInstanceResponse_nsLcmOpOccId :: Lens.Lens' UpdateSolNetworkInstanceResponse (Prelude.Maybe Prelude.Text)
updateSolNetworkInstanceResponse_nsLcmOpOccId = Lens.lens (\UpdateSolNetworkInstanceResponse' {nsLcmOpOccId} -> nsLcmOpOccId) (\s@UpdateSolNetworkInstanceResponse' {} a -> s {nsLcmOpOccId = a} :: UpdateSolNetworkInstanceResponse)

-- | A tag is a label that you assign to an Amazon Web Services resource.
-- Each tag consists of a key and an optional value. When you use this API,
-- the tags are transferred to the network operation that is created. Use
-- tags to search and filter your resources or track your Amazon Web
-- Services costs.
updateSolNetworkInstanceResponse_tags :: Lens.Lens' UpdateSolNetworkInstanceResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateSolNetworkInstanceResponse_tags = Lens.lens (\UpdateSolNetworkInstanceResponse' {tags} -> tags) (\s@UpdateSolNetworkInstanceResponse' {} a -> s {tags = a} :: UpdateSolNetworkInstanceResponse) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The response's http status code.
updateSolNetworkInstanceResponse_httpStatus :: Lens.Lens' UpdateSolNetworkInstanceResponse Prelude.Int
updateSolNetworkInstanceResponse_httpStatus = Lens.lens (\UpdateSolNetworkInstanceResponse' {httpStatus} -> httpStatus) (\s@UpdateSolNetworkInstanceResponse' {} a -> s {httpStatus = a} :: UpdateSolNetworkInstanceResponse)

instance
  Prelude.NFData
    UpdateSolNetworkInstanceResponse
  where
  rnf UpdateSolNetworkInstanceResponse' {..} =
    Prelude.rnf nsLcmOpOccId
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
