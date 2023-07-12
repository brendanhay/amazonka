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
-- Module      : Amazonka.GroundStation.DeleteDataflowEndpointGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a dataflow endpoint group.
module Amazonka.GroundStation.DeleteDataflowEndpointGroup
  ( -- * Creating a Request
    DeleteDataflowEndpointGroup (..),
    newDeleteDataflowEndpointGroup,

    -- * Request Lenses
    deleteDataflowEndpointGroup_dataflowEndpointGroupId,

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
-- /See:/ 'newDeleteDataflowEndpointGroup' smart constructor.
data DeleteDataflowEndpointGroup = DeleteDataflowEndpointGroup'
  { -- | UUID of a dataflow endpoint group.
    dataflowEndpointGroupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDataflowEndpointGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataflowEndpointGroupId', 'deleteDataflowEndpointGroup_dataflowEndpointGroupId' - UUID of a dataflow endpoint group.
newDeleteDataflowEndpointGroup ::
  -- | 'dataflowEndpointGroupId'
  Prelude.Text ->
  DeleteDataflowEndpointGroup
newDeleteDataflowEndpointGroup
  pDataflowEndpointGroupId_ =
    DeleteDataflowEndpointGroup'
      { dataflowEndpointGroupId =
          pDataflowEndpointGroupId_
      }

-- | UUID of a dataflow endpoint group.
deleteDataflowEndpointGroup_dataflowEndpointGroupId :: Lens.Lens' DeleteDataflowEndpointGroup Prelude.Text
deleteDataflowEndpointGroup_dataflowEndpointGroupId = Lens.lens (\DeleteDataflowEndpointGroup' {dataflowEndpointGroupId} -> dataflowEndpointGroupId) (\s@DeleteDataflowEndpointGroup' {} a -> s {dataflowEndpointGroupId = a} :: DeleteDataflowEndpointGroup)

instance Core.AWSRequest DeleteDataflowEndpointGroup where
  type
    AWSResponse DeleteDataflowEndpointGroup =
      DataflowEndpointGroupIdResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable DeleteDataflowEndpointGroup where
  hashWithSalt _salt DeleteDataflowEndpointGroup' {..} =
    _salt
      `Prelude.hashWithSalt` dataflowEndpointGroupId

instance Prelude.NFData DeleteDataflowEndpointGroup where
  rnf DeleteDataflowEndpointGroup' {..} =
    Prelude.rnf dataflowEndpointGroupId

instance Data.ToHeaders DeleteDataflowEndpointGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteDataflowEndpointGroup where
  toPath DeleteDataflowEndpointGroup' {..} =
    Prelude.mconcat
      [ "/dataflowEndpointGroup/",
        Data.toBS dataflowEndpointGroupId
      ]

instance Data.ToQuery DeleteDataflowEndpointGroup where
  toQuery = Prelude.const Prelude.mempty
