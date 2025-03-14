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
-- Module      : Amazonka.Connect.UpdateContactFlowName
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The name of the flow.
--
-- You can also create and update flows using the
-- <https://docs.aws.amazon.com/connect/latest/APIReference/flow-language.html Amazon Connect Flow language>.
module Amazonka.Connect.UpdateContactFlowName
  ( -- * Creating a Request
    UpdateContactFlowName (..),
    newUpdateContactFlowName,

    -- * Request Lenses
    updateContactFlowName_description,
    updateContactFlowName_name,
    updateContactFlowName_instanceId,
    updateContactFlowName_contactFlowId,

    -- * Destructuring the Response
    UpdateContactFlowNameResponse (..),
    newUpdateContactFlowNameResponse,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateContactFlowName' smart constructor.
data UpdateContactFlowName = UpdateContactFlowName'
  { -- | The description of the flow.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the flow.
    name :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Prelude.Text,
    -- | The identifier of the flow.
    contactFlowId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateContactFlowName' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateContactFlowName_description' - The description of the flow.
--
-- 'name', 'updateContactFlowName_name' - The name of the flow.
--
-- 'instanceId', 'updateContactFlowName_instanceId' - The identifier of the Amazon Connect instance.
--
-- 'contactFlowId', 'updateContactFlowName_contactFlowId' - The identifier of the flow.
newUpdateContactFlowName ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'contactFlowId'
  Prelude.Text ->
  UpdateContactFlowName
newUpdateContactFlowName pInstanceId_ pContactFlowId_ =
  UpdateContactFlowName'
    { description =
        Prelude.Nothing,
      name = Prelude.Nothing,
      instanceId = pInstanceId_,
      contactFlowId = pContactFlowId_
    }

-- | The description of the flow.
updateContactFlowName_description :: Lens.Lens' UpdateContactFlowName (Prelude.Maybe Prelude.Text)
updateContactFlowName_description = Lens.lens (\UpdateContactFlowName' {description} -> description) (\s@UpdateContactFlowName' {} a -> s {description = a} :: UpdateContactFlowName)

-- | The name of the flow.
updateContactFlowName_name :: Lens.Lens' UpdateContactFlowName (Prelude.Maybe Prelude.Text)
updateContactFlowName_name = Lens.lens (\UpdateContactFlowName' {name} -> name) (\s@UpdateContactFlowName' {} a -> s {name = a} :: UpdateContactFlowName)

-- | The identifier of the Amazon Connect instance.
updateContactFlowName_instanceId :: Lens.Lens' UpdateContactFlowName Prelude.Text
updateContactFlowName_instanceId = Lens.lens (\UpdateContactFlowName' {instanceId} -> instanceId) (\s@UpdateContactFlowName' {} a -> s {instanceId = a} :: UpdateContactFlowName)

-- | The identifier of the flow.
updateContactFlowName_contactFlowId :: Lens.Lens' UpdateContactFlowName Prelude.Text
updateContactFlowName_contactFlowId = Lens.lens (\UpdateContactFlowName' {contactFlowId} -> contactFlowId) (\s@UpdateContactFlowName' {} a -> s {contactFlowId = a} :: UpdateContactFlowName)

instance Core.AWSRequest UpdateContactFlowName where
  type
    AWSResponse UpdateContactFlowName =
      UpdateContactFlowNameResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull UpdateContactFlowNameResponse'

instance Prelude.Hashable UpdateContactFlowName where
  hashWithSalt _salt UpdateContactFlowName' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` contactFlowId

instance Prelude.NFData UpdateContactFlowName where
  rnf UpdateContactFlowName' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf name `Prelude.seq`
        Prelude.rnf instanceId `Prelude.seq`
          Prelude.rnf contactFlowId

instance Data.ToHeaders UpdateContactFlowName where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateContactFlowName where
  toJSON UpdateContactFlowName' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("Name" Data..=) Prelude.<$> name
          ]
      )

instance Data.ToPath UpdateContactFlowName where
  toPath UpdateContactFlowName' {..} =
    Prelude.mconcat
      [ "/contact-flows/",
        Data.toBS instanceId,
        "/",
        Data.toBS contactFlowId,
        "/name"
      ]

instance Data.ToQuery UpdateContactFlowName where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateContactFlowNameResponse' smart constructor.
data UpdateContactFlowNameResponse = UpdateContactFlowNameResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateContactFlowNameResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateContactFlowNameResponse ::
  UpdateContactFlowNameResponse
newUpdateContactFlowNameResponse =
  UpdateContactFlowNameResponse'

instance Prelude.NFData UpdateContactFlowNameResponse where
  rnf _ = ()
