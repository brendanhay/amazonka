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
-- Module      : Amazonka.Connect.DeleteTrafficDistributionGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a traffic distribution group. This API can be called only in the
-- Region where the traffic distribution group is created.
--
-- For more information about deleting traffic distribution groups, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/delete-traffic-distribution-groups.html Delete traffic distribution groups>
-- in the /Amazon Connect Administrator Guide/.
module Amazonka.Connect.DeleteTrafficDistributionGroup
  ( -- * Creating a Request
    DeleteTrafficDistributionGroup (..),
    newDeleteTrafficDistributionGroup,

    -- * Request Lenses
    deleteTrafficDistributionGroup_trafficDistributionGroupId,

    -- * Destructuring the Response
    DeleteTrafficDistributionGroupResponse (..),
    newDeleteTrafficDistributionGroupResponse,

    -- * Response Lenses
    deleteTrafficDistributionGroupResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteTrafficDistributionGroup' smart constructor.
data DeleteTrafficDistributionGroup = DeleteTrafficDistributionGroup'
  { -- | The identifier of the traffic distribution group. This can be the ID or
    -- the ARN if the API is being called in the Region where the traffic
    -- distribution group was created. The ARN must be provided if the call is
    -- from the replicated Region.
    trafficDistributionGroupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTrafficDistributionGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trafficDistributionGroupId', 'deleteTrafficDistributionGroup_trafficDistributionGroupId' - The identifier of the traffic distribution group. This can be the ID or
-- the ARN if the API is being called in the Region where the traffic
-- distribution group was created. The ARN must be provided if the call is
-- from the replicated Region.
newDeleteTrafficDistributionGroup ::
  -- | 'trafficDistributionGroupId'
  Prelude.Text ->
  DeleteTrafficDistributionGroup
newDeleteTrafficDistributionGroup
  pTrafficDistributionGroupId_ =
    DeleteTrafficDistributionGroup'
      { trafficDistributionGroupId =
          pTrafficDistributionGroupId_
      }

-- | The identifier of the traffic distribution group. This can be the ID or
-- the ARN if the API is being called in the Region where the traffic
-- distribution group was created. The ARN must be provided if the call is
-- from the replicated Region.
deleteTrafficDistributionGroup_trafficDistributionGroupId :: Lens.Lens' DeleteTrafficDistributionGroup Prelude.Text
deleteTrafficDistributionGroup_trafficDistributionGroupId = Lens.lens (\DeleteTrafficDistributionGroup' {trafficDistributionGroupId} -> trafficDistributionGroupId) (\s@DeleteTrafficDistributionGroup' {} a -> s {trafficDistributionGroupId = a} :: DeleteTrafficDistributionGroup)

instance
  Core.AWSRequest
    DeleteTrafficDistributionGroup
  where
  type
    AWSResponse DeleteTrafficDistributionGroup =
      DeleteTrafficDistributionGroupResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteTrafficDistributionGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteTrafficDistributionGroup
  where
  hashWithSalt
    _salt
    DeleteTrafficDistributionGroup' {..} =
      _salt
        `Prelude.hashWithSalt` trafficDistributionGroupId

instance
  Prelude.NFData
    DeleteTrafficDistributionGroup
  where
  rnf DeleteTrafficDistributionGroup' {..} =
    Prelude.rnf trafficDistributionGroupId

instance
  Data.ToHeaders
    DeleteTrafficDistributionGroup
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

instance Data.ToPath DeleteTrafficDistributionGroup where
  toPath DeleteTrafficDistributionGroup' {..} =
    Prelude.mconcat
      [ "/traffic-distribution-group/",
        Data.toBS trafficDistributionGroupId
      ]

instance Data.ToQuery DeleteTrafficDistributionGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteTrafficDistributionGroupResponse' smart constructor.
data DeleteTrafficDistributionGroupResponse = DeleteTrafficDistributionGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTrafficDistributionGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteTrafficDistributionGroupResponse_httpStatus' - The response's http status code.
newDeleteTrafficDistributionGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteTrafficDistributionGroupResponse
newDeleteTrafficDistributionGroupResponse
  pHttpStatus_ =
    DeleteTrafficDistributionGroupResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
deleteTrafficDistributionGroupResponse_httpStatus :: Lens.Lens' DeleteTrafficDistributionGroupResponse Prelude.Int
deleteTrafficDistributionGroupResponse_httpStatus = Lens.lens (\DeleteTrafficDistributionGroupResponse' {httpStatus} -> httpStatus) (\s@DeleteTrafficDistributionGroupResponse' {} a -> s {httpStatus = a} :: DeleteTrafficDistributionGroupResponse)

instance
  Prelude.NFData
    DeleteTrafficDistributionGroupResponse
  where
  rnf DeleteTrafficDistributionGroupResponse' {..} =
    Prelude.rnf httpStatus
