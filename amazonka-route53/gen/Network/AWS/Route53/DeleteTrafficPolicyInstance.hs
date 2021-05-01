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
-- Module      : Network.AWS.Route53.DeleteTrafficPolicyInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a traffic policy instance and all of the resource record sets
-- that Amazon Route 53 created when you created the instance.
--
-- In the Route 53 console, traffic policy instances are known as policy
-- records.
module Network.AWS.Route53.DeleteTrafficPolicyInstance
  ( -- * Creating a Request
    DeleteTrafficPolicyInstance (..),
    newDeleteTrafficPolicyInstance,

    -- * Request Lenses
    deleteTrafficPolicyInstance_id,

    -- * Destructuring the Response
    DeleteTrafficPolicyInstanceResponse (..),
    newDeleteTrafficPolicyInstanceResponse,

    -- * Response Lenses
    deleteTrafficPolicyInstanceResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53.Types

-- | A request to delete a specified traffic policy instance.
--
-- /See:/ 'newDeleteTrafficPolicyInstance' smart constructor.
data DeleteTrafficPolicyInstance = DeleteTrafficPolicyInstance'
  { -- | The ID of the traffic policy instance that you want to delete.
    --
    -- When you delete a traffic policy instance, Amazon Route 53 also deletes
    -- all of the resource record sets that were created when you created the
    -- traffic policy instance.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteTrafficPolicyInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'deleteTrafficPolicyInstance_id' - The ID of the traffic policy instance that you want to delete.
--
-- When you delete a traffic policy instance, Amazon Route 53 also deletes
-- all of the resource record sets that were created when you created the
-- traffic policy instance.
newDeleteTrafficPolicyInstance ::
  -- | 'id'
  Prelude.Text ->
  DeleteTrafficPolicyInstance
newDeleteTrafficPolicyInstance pId_ =
  DeleteTrafficPolicyInstance' {id = pId_}

-- | The ID of the traffic policy instance that you want to delete.
--
-- When you delete a traffic policy instance, Amazon Route 53 also deletes
-- all of the resource record sets that were created when you created the
-- traffic policy instance.
deleteTrafficPolicyInstance_id :: Lens.Lens' DeleteTrafficPolicyInstance Prelude.Text
deleteTrafficPolicyInstance_id = Lens.lens (\DeleteTrafficPolicyInstance' {id} -> id) (\s@DeleteTrafficPolicyInstance' {} a -> s {id = a} :: DeleteTrafficPolicyInstance)

instance
  Prelude.AWSRequest
    DeleteTrafficPolicyInstance
  where
  type
    Rs DeleteTrafficPolicyInstance =
      DeleteTrafficPolicyInstanceResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteTrafficPolicyInstanceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteTrafficPolicyInstance

instance Prelude.NFData DeleteTrafficPolicyInstance

instance
  Prelude.ToHeaders
    DeleteTrafficPolicyInstance
  where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteTrafficPolicyInstance where
  toPath DeleteTrafficPolicyInstance' {..} =
    Prelude.mconcat
      [ "/2013-04-01/trafficpolicyinstance/",
        Prelude.toBS id
      ]

instance Prelude.ToQuery DeleteTrafficPolicyInstance where
  toQuery = Prelude.const Prelude.mempty

-- | An empty element.
--
-- /See:/ 'newDeleteTrafficPolicyInstanceResponse' smart constructor.
data DeleteTrafficPolicyInstanceResponse = DeleteTrafficPolicyInstanceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteTrafficPolicyInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteTrafficPolicyInstanceResponse_httpStatus' - The response's http status code.
newDeleteTrafficPolicyInstanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteTrafficPolicyInstanceResponse
newDeleteTrafficPolicyInstanceResponse pHttpStatus_ =
  DeleteTrafficPolicyInstanceResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteTrafficPolicyInstanceResponse_httpStatus :: Lens.Lens' DeleteTrafficPolicyInstanceResponse Prelude.Int
deleteTrafficPolicyInstanceResponse_httpStatus = Lens.lens (\DeleteTrafficPolicyInstanceResponse' {httpStatus} -> httpStatus) (\s@DeleteTrafficPolicyInstanceResponse' {} a -> s {httpStatus = a} :: DeleteTrafficPolicyInstanceResponse)

instance
  Prelude.NFData
    DeleteTrafficPolicyInstanceResponse
