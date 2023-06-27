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
-- Module      : Amazonka.GroundStation.CreateDataflowEndpointGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a @DataflowEndpoint@ group containing the specified list of
-- @DataflowEndpoint@ objects.
--
-- The @name@ field in each endpoint is used in your mission profile
-- @DataflowEndpointConfig@ to specify which endpoints to use during a
-- contact.
--
-- When a contact uses multiple @DataflowEndpointConfig@ objects, each
-- @Config@ must match a @DataflowEndpoint@ in the same group.
module Amazonka.GroundStation.CreateDataflowEndpointGroup
  ( -- * Creating a Request
    CreateDataflowEndpointGroup (..),
    newCreateDataflowEndpointGroup,

    -- * Request Lenses
    createDataflowEndpointGroup_contactPostPassDurationSeconds,
    createDataflowEndpointGroup_contactPrePassDurationSeconds,
    createDataflowEndpointGroup_tags,
    createDataflowEndpointGroup_endpointDetails,

    -- * Destructuring the Response
    DataflowEndpointGroupIdResponse (..),
    newDataflowEndpointGroupIdResponse,

    -- * Response Lenses
    dataflowEndpointGroupIdResponse_dataflowEndpointGroupId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GroundStation.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newCreateDataflowEndpointGroup' smart constructor.
data CreateDataflowEndpointGroup = CreateDataflowEndpointGroup'
  { -- | Amount of time, in seconds, after a contact ends that the Ground Station
    -- Dataflow Endpoint Group will be in a @POSTPASS@ state. A Ground Station
    -- Dataflow Endpoint Group State Change event will be emitted when the
    -- Dataflow Endpoint Group enters and exits the @POSTPASS@ state.
    contactPostPassDurationSeconds :: Prelude.Maybe Prelude.Natural,
    -- | Amount of time, in seconds, before a contact starts that the Ground
    -- Station Dataflow Endpoint Group will be in a @PREPASS@ state. A Ground
    -- Station Dataflow Endpoint Group State Change event will be emitted when
    -- the Dataflow Endpoint Group enters and exits the @PREPASS@ state.
    contactPrePassDurationSeconds :: Prelude.Maybe Prelude.Natural,
    -- | Tags of a dataflow endpoint group.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Endpoint details of each endpoint in the dataflow endpoint group.
    endpointDetails :: [EndpointDetails]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDataflowEndpointGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contactPostPassDurationSeconds', 'createDataflowEndpointGroup_contactPostPassDurationSeconds' - Amount of time, in seconds, after a contact ends that the Ground Station
-- Dataflow Endpoint Group will be in a @POSTPASS@ state. A Ground Station
-- Dataflow Endpoint Group State Change event will be emitted when the
-- Dataflow Endpoint Group enters and exits the @POSTPASS@ state.
--
-- 'contactPrePassDurationSeconds', 'createDataflowEndpointGroup_contactPrePassDurationSeconds' - Amount of time, in seconds, before a contact starts that the Ground
-- Station Dataflow Endpoint Group will be in a @PREPASS@ state. A Ground
-- Station Dataflow Endpoint Group State Change event will be emitted when
-- the Dataflow Endpoint Group enters and exits the @PREPASS@ state.
--
-- 'tags', 'createDataflowEndpointGroup_tags' - Tags of a dataflow endpoint group.
--
-- 'endpointDetails', 'createDataflowEndpointGroup_endpointDetails' - Endpoint details of each endpoint in the dataflow endpoint group.
newCreateDataflowEndpointGroup ::
  CreateDataflowEndpointGroup
newCreateDataflowEndpointGroup =
  CreateDataflowEndpointGroup'
    { contactPostPassDurationSeconds =
        Prelude.Nothing,
      contactPrePassDurationSeconds =
        Prelude.Nothing,
      tags = Prelude.Nothing,
      endpointDetails = Prelude.mempty
    }

-- | Amount of time, in seconds, after a contact ends that the Ground Station
-- Dataflow Endpoint Group will be in a @POSTPASS@ state. A Ground Station
-- Dataflow Endpoint Group State Change event will be emitted when the
-- Dataflow Endpoint Group enters and exits the @POSTPASS@ state.
createDataflowEndpointGroup_contactPostPassDurationSeconds :: Lens.Lens' CreateDataflowEndpointGroup (Prelude.Maybe Prelude.Natural)
createDataflowEndpointGroup_contactPostPassDurationSeconds = Lens.lens (\CreateDataflowEndpointGroup' {contactPostPassDurationSeconds} -> contactPostPassDurationSeconds) (\s@CreateDataflowEndpointGroup' {} a -> s {contactPostPassDurationSeconds = a} :: CreateDataflowEndpointGroup)

-- | Amount of time, in seconds, before a contact starts that the Ground
-- Station Dataflow Endpoint Group will be in a @PREPASS@ state. A Ground
-- Station Dataflow Endpoint Group State Change event will be emitted when
-- the Dataflow Endpoint Group enters and exits the @PREPASS@ state.
createDataflowEndpointGroup_contactPrePassDurationSeconds :: Lens.Lens' CreateDataflowEndpointGroup (Prelude.Maybe Prelude.Natural)
createDataflowEndpointGroup_contactPrePassDurationSeconds = Lens.lens (\CreateDataflowEndpointGroup' {contactPrePassDurationSeconds} -> contactPrePassDurationSeconds) (\s@CreateDataflowEndpointGroup' {} a -> s {contactPrePassDurationSeconds = a} :: CreateDataflowEndpointGroup)

-- | Tags of a dataflow endpoint group.
createDataflowEndpointGroup_tags :: Lens.Lens' CreateDataflowEndpointGroup (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createDataflowEndpointGroup_tags = Lens.lens (\CreateDataflowEndpointGroup' {tags} -> tags) (\s@CreateDataflowEndpointGroup' {} a -> s {tags = a} :: CreateDataflowEndpointGroup) Prelude.. Lens.mapping Lens.coerced

-- | Endpoint details of each endpoint in the dataflow endpoint group.
createDataflowEndpointGroup_endpointDetails :: Lens.Lens' CreateDataflowEndpointGroup [EndpointDetails]
createDataflowEndpointGroup_endpointDetails = Lens.lens (\CreateDataflowEndpointGroup' {endpointDetails} -> endpointDetails) (\s@CreateDataflowEndpointGroup' {} a -> s {endpointDetails = a} :: CreateDataflowEndpointGroup) Prelude.. Lens.coerced

instance Core.AWSRequest CreateDataflowEndpointGroup where
  type
    AWSResponse CreateDataflowEndpointGroup =
      DataflowEndpointGroupIdResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable CreateDataflowEndpointGroup where
  hashWithSalt _salt CreateDataflowEndpointGroup' {..} =
    _salt
      `Prelude.hashWithSalt` contactPostPassDurationSeconds
      `Prelude.hashWithSalt` contactPrePassDurationSeconds
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` endpointDetails

instance Prelude.NFData CreateDataflowEndpointGroup where
  rnf CreateDataflowEndpointGroup' {..} =
    Prelude.rnf contactPostPassDurationSeconds
      `Prelude.seq` Prelude.rnf contactPrePassDurationSeconds
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf endpointDetails

instance Data.ToHeaders CreateDataflowEndpointGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateDataflowEndpointGroup where
  toJSON CreateDataflowEndpointGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("contactPostPassDurationSeconds" Data..=)
              Prelude.<$> contactPostPassDurationSeconds,
            ("contactPrePassDurationSeconds" Data..=)
              Prelude.<$> contactPrePassDurationSeconds,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("endpointDetails" Data..= endpointDetails)
          ]
      )

instance Data.ToPath CreateDataflowEndpointGroup where
  toPath = Prelude.const "/dataflowEndpointGroup"

instance Data.ToQuery CreateDataflowEndpointGroup where
  toQuery = Prelude.const Prelude.mempty
