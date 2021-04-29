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
-- Module      : Network.AWS.Connect.UpdateQuickConnectName
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Updates the name and description of a quick connect. The request accepts
-- the following data in JSON format. At least @Name@ or @Description@ must
-- be provided.
module Network.AWS.Connect.UpdateQuickConnectName
  ( -- * Creating a Request
    UpdateQuickConnectName (..),
    newUpdateQuickConnectName,

    -- * Request Lenses
    updateQuickConnectName_name,
    updateQuickConnectName_description,
    updateQuickConnectName_instanceId,
    updateQuickConnectName_quickConnectId,

    -- * Destructuring the Response
    UpdateQuickConnectNameResponse (..),
    newUpdateQuickConnectNameResponse,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateQuickConnectName' smart constructor.
data UpdateQuickConnectName = UpdateQuickConnectName'
  { -- | The name of the quick connect.
    name :: Prelude.Maybe Prelude.Text,
    -- | The description of the quick connect.
    description :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Prelude.Text,
    -- | The identifier for the quick connect.
    quickConnectId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateQuickConnectName' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateQuickConnectName_name' - The name of the quick connect.
--
-- 'description', 'updateQuickConnectName_description' - The description of the quick connect.
--
-- 'instanceId', 'updateQuickConnectName_instanceId' - The identifier of the Amazon Connect instance.
--
-- 'quickConnectId', 'updateQuickConnectName_quickConnectId' - The identifier for the quick connect.
newUpdateQuickConnectName ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'quickConnectId'
  Prelude.Text ->
  UpdateQuickConnectName
newUpdateQuickConnectName
  pInstanceId_
  pQuickConnectId_ =
    UpdateQuickConnectName'
      { name = Prelude.Nothing,
        description = Prelude.Nothing,
        instanceId = pInstanceId_,
        quickConnectId = pQuickConnectId_
      }

-- | The name of the quick connect.
updateQuickConnectName_name :: Lens.Lens' UpdateQuickConnectName (Prelude.Maybe Prelude.Text)
updateQuickConnectName_name = Lens.lens (\UpdateQuickConnectName' {name} -> name) (\s@UpdateQuickConnectName' {} a -> s {name = a} :: UpdateQuickConnectName)

-- | The description of the quick connect.
updateQuickConnectName_description :: Lens.Lens' UpdateQuickConnectName (Prelude.Maybe Prelude.Text)
updateQuickConnectName_description = Lens.lens (\UpdateQuickConnectName' {description} -> description) (\s@UpdateQuickConnectName' {} a -> s {description = a} :: UpdateQuickConnectName)

-- | The identifier of the Amazon Connect instance.
updateQuickConnectName_instanceId :: Lens.Lens' UpdateQuickConnectName Prelude.Text
updateQuickConnectName_instanceId = Lens.lens (\UpdateQuickConnectName' {instanceId} -> instanceId) (\s@UpdateQuickConnectName' {} a -> s {instanceId = a} :: UpdateQuickConnectName)

-- | The identifier for the quick connect.
updateQuickConnectName_quickConnectId :: Lens.Lens' UpdateQuickConnectName Prelude.Text
updateQuickConnectName_quickConnectId = Lens.lens (\UpdateQuickConnectName' {quickConnectId} -> quickConnectId) (\s@UpdateQuickConnectName' {} a -> s {quickConnectId = a} :: UpdateQuickConnectName)

instance Prelude.AWSRequest UpdateQuickConnectName where
  type
    Rs UpdateQuickConnectName =
      UpdateQuickConnectNameResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      UpdateQuickConnectNameResponse'

instance Prelude.Hashable UpdateQuickConnectName

instance Prelude.NFData UpdateQuickConnectName

instance Prelude.ToHeaders UpdateQuickConnectName where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateQuickConnectName where
  toJSON UpdateQuickConnectName' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Name" Prelude..=) Prelude.<$> name,
            ("Description" Prelude..=) Prelude.<$> description
          ]
      )

instance Prelude.ToPath UpdateQuickConnectName where
  toPath UpdateQuickConnectName' {..} =
    Prelude.mconcat
      [ "/quick-connects/",
        Prelude.toBS instanceId,
        "/",
        Prelude.toBS quickConnectId,
        "/name"
      ]

instance Prelude.ToQuery UpdateQuickConnectName where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateQuickConnectNameResponse' smart constructor.
data UpdateQuickConnectNameResponse = UpdateQuickConnectNameResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateQuickConnectNameResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateQuickConnectNameResponse ::
  UpdateQuickConnectNameResponse
newUpdateQuickConnectNameResponse =
  UpdateQuickConnectNameResponse'

instance
  Prelude.NFData
    UpdateQuickConnectNameResponse
