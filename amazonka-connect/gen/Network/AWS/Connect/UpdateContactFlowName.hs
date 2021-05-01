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
-- Module      : Network.AWS.Connect.UpdateContactFlowName
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The name of the contact flow.
--
-- You can also create and update contact flows using the
-- <https://docs.aws.amazon.com/connect/latest/adminguide/flow-language.html Amazon Connect Flow language>.
module Network.AWS.Connect.UpdateContactFlowName
  ( -- * Creating a Request
    UpdateContactFlowName (..),
    newUpdateContactFlowName,

    -- * Request Lenses
    updateContactFlowName_name,
    updateContactFlowName_description,
    updateContactFlowName_instanceId,
    updateContactFlowName_contactFlowId,

    -- * Destructuring the Response
    UpdateContactFlowNameResponse (..),
    newUpdateContactFlowNameResponse,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateContactFlowName' smart constructor.
data UpdateContactFlowName = UpdateContactFlowName'
  { -- | The name of the contact flow.
    name :: Prelude.Maybe Prelude.Text,
    -- | The description of the contact flow.
    description :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Prelude.Text,
    -- | The identifier of the contact flow.
    contactFlowId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateContactFlowName' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateContactFlowName_name' - The name of the contact flow.
--
-- 'description', 'updateContactFlowName_description' - The description of the contact flow.
--
-- 'instanceId', 'updateContactFlowName_instanceId' - The identifier of the Amazon Connect instance.
--
-- 'contactFlowId', 'updateContactFlowName_contactFlowId' - The identifier of the contact flow.
newUpdateContactFlowName ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'contactFlowId'
  Prelude.Text ->
  UpdateContactFlowName
newUpdateContactFlowName pInstanceId_ pContactFlowId_ =
  UpdateContactFlowName'
    { name = Prelude.Nothing,
      description = Prelude.Nothing,
      instanceId = pInstanceId_,
      contactFlowId = pContactFlowId_
    }

-- | The name of the contact flow.
updateContactFlowName_name :: Lens.Lens' UpdateContactFlowName (Prelude.Maybe Prelude.Text)
updateContactFlowName_name = Lens.lens (\UpdateContactFlowName' {name} -> name) (\s@UpdateContactFlowName' {} a -> s {name = a} :: UpdateContactFlowName)

-- | The description of the contact flow.
updateContactFlowName_description :: Lens.Lens' UpdateContactFlowName (Prelude.Maybe Prelude.Text)
updateContactFlowName_description = Lens.lens (\UpdateContactFlowName' {description} -> description) (\s@UpdateContactFlowName' {} a -> s {description = a} :: UpdateContactFlowName)

-- | The identifier of the Amazon Connect instance.
updateContactFlowName_instanceId :: Lens.Lens' UpdateContactFlowName Prelude.Text
updateContactFlowName_instanceId = Lens.lens (\UpdateContactFlowName' {instanceId} -> instanceId) (\s@UpdateContactFlowName' {} a -> s {instanceId = a} :: UpdateContactFlowName)

-- | The identifier of the contact flow.
updateContactFlowName_contactFlowId :: Lens.Lens' UpdateContactFlowName Prelude.Text
updateContactFlowName_contactFlowId = Lens.lens (\UpdateContactFlowName' {contactFlowId} -> contactFlowId) (\s@UpdateContactFlowName' {} a -> s {contactFlowId = a} :: UpdateContactFlowName)

instance Prelude.AWSRequest UpdateContactFlowName where
  type
    Rs UpdateContactFlowName =
      UpdateContactFlowNameResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull UpdateContactFlowNameResponse'

instance Prelude.Hashable UpdateContactFlowName

instance Prelude.NFData UpdateContactFlowName

instance Prelude.ToHeaders UpdateContactFlowName where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateContactFlowName where
  toJSON UpdateContactFlowName' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Name" Prelude..=) Prelude.<$> name,
            ("Description" Prelude..=) Prelude.<$> description
          ]
      )

instance Prelude.ToPath UpdateContactFlowName where
  toPath UpdateContactFlowName' {..} =
    Prelude.mconcat
      [ "/contact-flows/",
        Prelude.toBS instanceId,
        "/",
        Prelude.toBS contactFlowId,
        "/name"
      ]

instance Prelude.ToQuery UpdateContactFlowName where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateContactFlowNameResponse' smart constructor.
data UpdateContactFlowNameResponse = UpdateContactFlowNameResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateContactFlowNameResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateContactFlowNameResponse ::
  UpdateContactFlowNameResponse
newUpdateContactFlowNameResponse =
  UpdateContactFlowNameResponse'

instance Prelude.NFData UpdateContactFlowNameResponse
