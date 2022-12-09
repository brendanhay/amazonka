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
-- Module      : Amazonka.Connect.UpdateRoutingProfileName
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the name and description of a routing profile. The request
-- accepts the following data in JSON format. At least @Name@ or
-- @Description@ must be provided.
module Amazonka.Connect.UpdateRoutingProfileName
  ( -- * Creating a Request
    UpdateRoutingProfileName (..),
    newUpdateRoutingProfileName,

    -- * Request Lenses
    updateRoutingProfileName_description,
    updateRoutingProfileName_name,
    updateRoutingProfileName_instanceId,
    updateRoutingProfileName_routingProfileId,

    -- * Destructuring the Response
    UpdateRoutingProfileNameResponse (..),
    newUpdateRoutingProfileNameResponse,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateRoutingProfileName' smart constructor.
data UpdateRoutingProfileName = UpdateRoutingProfileName'
  { -- | The description of the routing profile. Must not be more than 250
    -- characters.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the routing profile. Must not be more than 127 characters.
    name :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | The identifier of the routing profile.
    routingProfileId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRoutingProfileName' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateRoutingProfileName_description' - The description of the routing profile. Must not be more than 250
-- characters.
--
-- 'name', 'updateRoutingProfileName_name' - The name of the routing profile. Must not be more than 127 characters.
--
-- 'instanceId', 'updateRoutingProfileName_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'routingProfileId', 'updateRoutingProfileName_routingProfileId' - The identifier of the routing profile.
newUpdateRoutingProfileName ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'routingProfileId'
  Prelude.Text ->
  UpdateRoutingProfileName
newUpdateRoutingProfileName
  pInstanceId_
  pRoutingProfileId_ =
    UpdateRoutingProfileName'
      { description =
          Prelude.Nothing,
        name = Prelude.Nothing,
        instanceId = pInstanceId_,
        routingProfileId = pRoutingProfileId_
      }

-- | The description of the routing profile. Must not be more than 250
-- characters.
updateRoutingProfileName_description :: Lens.Lens' UpdateRoutingProfileName (Prelude.Maybe Prelude.Text)
updateRoutingProfileName_description = Lens.lens (\UpdateRoutingProfileName' {description} -> description) (\s@UpdateRoutingProfileName' {} a -> s {description = a} :: UpdateRoutingProfileName)

-- | The name of the routing profile. Must not be more than 127 characters.
updateRoutingProfileName_name :: Lens.Lens' UpdateRoutingProfileName (Prelude.Maybe Prelude.Text)
updateRoutingProfileName_name = Lens.lens (\UpdateRoutingProfileName' {name} -> name) (\s@UpdateRoutingProfileName' {} a -> s {name = a} :: UpdateRoutingProfileName)

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
updateRoutingProfileName_instanceId :: Lens.Lens' UpdateRoutingProfileName Prelude.Text
updateRoutingProfileName_instanceId = Lens.lens (\UpdateRoutingProfileName' {instanceId} -> instanceId) (\s@UpdateRoutingProfileName' {} a -> s {instanceId = a} :: UpdateRoutingProfileName)

-- | The identifier of the routing profile.
updateRoutingProfileName_routingProfileId :: Lens.Lens' UpdateRoutingProfileName Prelude.Text
updateRoutingProfileName_routingProfileId = Lens.lens (\UpdateRoutingProfileName' {routingProfileId} -> routingProfileId) (\s@UpdateRoutingProfileName' {} a -> s {routingProfileId = a} :: UpdateRoutingProfileName)

instance Core.AWSRequest UpdateRoutingProfileName where
  type
    AWSResponse UpdateRoutingProfileName =
      UpdateRoutingProfileNameResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      UpdateRoutingProfileNameResponse'

instance Prelude.Hashable UpdateRoutingProfileName where
  hashWithSalt _salt UpdateRoutingProfileName' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` routingProfileId

instance Prelude.NFData UpdateRoutingProfileName where
  rnf UpdateRoutingProfileName' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf routingProfileId

instance Data.ToHeaders UpdateRoutingProfileName where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateRoutingProfileName where
  toJSON UpdateRoutingProfileName' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("Name" Data..=) Prelude.<$> name
          ]
      )

instance Data.ToPath UpdateRoutingProfileName where
  toPath UpdateRoutingProfileName' {..} =
    Prelude.mconcat
      [ "/routing-profiles/",
        Data.toBS instanceId,
        "/",
        Data.toBS routingProfileId,
        "/name"
      ]

instance Data.ToQuery UpdateRoutingProfileName where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateRoutingProfileNameResponse' smart constructor.
data UpdateRoutingProfileNameResponse = UpdateRoutingProfileNameResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRoutingProfileNameResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateRoutingProfileNameResponse ::
  UpdateRoutingProfileNameResponse
newUpdateRoutingProfileNameResponse =
  UpdateRoutingProfileNameResponse'

instance
  Prelude.NFData
    UpdateRoutingProfileNameResponse
  where
  rnf _ = ()
