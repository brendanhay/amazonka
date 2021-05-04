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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The DELETE request to delete a usage plan of a given plan Id.
--
-- /See:/ 'newDeleteUsagePlan' smart constructor.
data DeleteUsagePlan = DeleteUsagePlan'
  { -- | [Required] The Id of the to-be-deleted usage plan.
    usagePlanId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DeleteUsagePlan
newDeleteUsagePlan pUsagePlanId_ =
  DeleteUsagePlan' {usagePlanId = pUsagePlanId_}

-- | [Required] The Id of the to-be-deleted usage plan.
deleteUsagePlan_usagePlanId :: Lens.Lens' DeleteUsagePlan Prelude.Text
deleteUsagePlan_usagePlanId = Lens.lens (\DeleteUsagePlan' {usagePlanId} -> usagePlanId) (\s@DeleteUsagePlan' {} a -> s {usagePlanId = a} :: DeleteUsagePlan)

instance Prelude.AWSRequest DeleteUsagePlan where
  type Rs DeleteUsagePlan = DeleteUsagePlanResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull DeleteUsagePlanResponse'

instance Prelude.Hashable DeleteUsagePlan

instance Prelude.NFData DeleteUsagePlan

instance Prelude.ToHeaders DeleteUsagePlan where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Prelude.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Prelude.ToPath DeleteUsagePlan where
  toPath DeleteUsagePlan' {..} =
    Prelude.mconcat
      ["/usageplans/", Prelude.toBS usagePlanId]

instance Prelude.ToQuery DeleteUsagePlan where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteUsagePlanResponse' smart constructor.
data DeleteUsagePlanResponse = DeleteUsagePlanResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteUsagePlanResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteUsagePlanResponse ::
  DeleteUsagePlanResponse
newDeleteUsagePlanResponse = DeleteUsagePlanResponse'

instance Prelude.NFData DeleteUsagePlanResponse
