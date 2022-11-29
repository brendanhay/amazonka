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
-- Module      : Amazonka.APIGateway.DeleteUsagePlan
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a usage plan of a given plan Id.
module Amazonka.APIGateway.DeleteUsagePlan
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

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The DELETE request to delete a usage plan of a given plan Id.
--
-- /See:/ 'newDeleteUsagePlan' smart constructor.
data DeleteUsagePlan = DeleteUsagePlan'
  { -- | The Id of the to-be-deleted usage plan.
    usagePlanId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteUsagePlan' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'usagePlanId', 'deleteUsagePlan_usagePlanId' - The Id of the to-be-deleted usage plan.
newDeleteUsagePlan ::
  -- | 'usagePlanId'
  Prelude.Text ->
  DeleteUsagePlan
newDeleteUsagePlan pUsagePlanId_ =
  DeleteUsagePlan' {usagePlanId = pUsagePlanId_}

-- | The Id of the to-be-deleted usage plan.
deleteUsagePlan_usagePlanId :: Lens.Lens' DeleteUsagePlan Prelude.Text
deleteUsagePlan_usagePlanId = Lens.lens (\DeleteUsagePlan' {usagePlanId} -> usagePlanId) (\s@DeleteUsagePlan' {} a -> s {usagePlanId = a} :: DeleteUsagePlan)

instance Core.AWSRequest DeleteUsagePlan where
  type
    AWSResponse DeleteUsagePlan =
      DeleteUsagePlanResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteUsagePlanResponse'

instance Prelude.Hashable DeleteUsagePlan where
  hashWithSalt _salt DeleteUsagePlan' {..} =
    _salt `Prelude.hashWithSalt` usagePlanId

instance Prelude.NFData DeleteUsagePlan where
  rnf DeleteUsagePlan' {..} = Prelude.rnf usagePlanId

instance Core.ToHeaders DeleteUsagePlan where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Core.ToPath DeleteUsagePlan where
  toPath DeleteUsagePlan' {..} =
    Prelude.mconcat
      ["/usageplans/", Core.toBS usagePlanId]

instance Core.ToQuery DeleteUsagePlan where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteUsagePlanResponse' smart constructor.
data DeleteUsagePlanResponse = DeleteUsagePlanResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteUsagePlanResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteUsagePlanResponse ::
  DeleteUsagePlanResponse
newDeleteUsagePlanResponse = DeleteUsagePlanResponse'

instance Prelude.NFData DeleteUsagePlanResponse where
  rnf _ = ()
