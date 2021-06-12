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
-- Module      : Network.AWS.APIGateway.DeleteUsagePlan
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a usage plan of a given plan Id.
module Network.AWS.APIGateway.DeleteUsagePlan
  ( -- * Creating a Request
    DeleteUsagePlan (..),
    newDeleteUsagePlan,

    -- * Request Lenses
    deleteUsagePlan_usagePlanId,

    -- * Destructuring the Response
    DeleteUsagePlanResponse (..),
    newDeleteUsagePlanResponse,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The DELETE request to delete a usage plan of a given plan Id.
--
-- /See:/ 'newDeleteUsagePlan' smart constructor.
data DeleteUsagePlan = DeleteUsagePlan'
  { -- | [Required] The Id of the to-be-deleted usage plan.
    usagePlanId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteUsagePlan' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'usagePlanId', 'deleteUsagePlan_usagePlanId' - [Required] The Id of the to-be-deleted usage plan.
newDeleteUsagePlan ::
  -- | 'usagePlanId'
  Core.Text ->
  DeleteUsagePlan
newDeleteUsagePlan pUsagePlanId_ =
  DeleteUsagePlan' {usagePlanId = pUsagePlanId_}

-- | [Required] The Id of the to-be-deleted usage plan.
deleteUsagePlan_usagePlanId :: Lens.Lens' DeleteUsagePlan Core.Text
deleteUsagePlan_usagePlanId = Lens.lens (\DeleteUsagePlan' {usagePlanId} -> usagePlanId) (\s@DeleteUsagePlan' {} a -> s {usagePlanId = a} :: DeleteUsagePlan)

instance Core.AWSRequest DeleteUsagePlan where
  type
    AWSResponse DeleteUsagePlan =
      DeleteUsagePlanResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull DeleteUsagePlanResponse'

instance Core.Hashable DeleteUsagePlan

instance Core.NFData DeleteUsagePlan

instance Core.ToHeaders DeleteUsagePlan where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Core.ByteString)
          ]
      )

instance Core.ToPath DeleteUsagePlan where
  toPath DeleteUsagePlan' {..} =
    Core.mconcat
      ["/usageplans/", Core.toBS usagePlanId]

instance Core.ToQuery DeleteUsagePlan where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteUsagePlanResponse' smart constructor.
data DeleteUsagePlanResponse = DeleteUsagePlanResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteUsagePlanResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteUsagePlanResponse ::
  DeleteUsagePlanResponse
newDeleteUsagePlanResponse = DeleteUsagePlanResponse'

instance Core.NFData DeleteUsagePlanResponse
