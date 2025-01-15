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
-- Module      : Amazonka.AutoScaling.DeleteWarmPool
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the warm pool for the specified Auto Scaling group.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/ec2-auto-scaling-warm-pools.html Warm pools for Amazon EC2 Auto Scaling>
-- in the /Amazon EC2 Auto Scaling User Guide/.
module Amazonka.AutoScaling.DeleteWarmPool
  ( -- * Creating a Request
    DeleteWarmPool (..),
    newDeleteWarmPool,

    -- * Request Lenses
    deleteWarmPool_forceDelete,
    deleteWarmPool_autoScalingGroupName,

    -- * Destructuring the Response
    DeleteWarmPoolResponse (..),
    newDeleteWarmPoolResponse,

    -- * Response Lenses
    deleteWarmPoolResponse_httpStatus,
  )
where

import Amazonka.AutoScaling.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteWarmPool' smart constructor.
data DeleteWarmPool = DeleteWarmPool'
  { -- | Specifies that the warm pool is to be deleted along with all of its
    -- associated instances, without waiting for all instances to be
    -- terminated. This parameter also deletes any outstanding lifecycle
    -- actions associated with the warm pool instances.
    forceDelete :: Prelude.Maybe Prelude.Bool,
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteWarmPool' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'forceDelete', 'deleteWarmPool_forceDelete' - Specifies that the warm pool is to be deleted along with all of its
-- associated instances, without waiting for all instances to be
-- terminated. This parameter also deletes any outstanding lifecycle
-- actions associated with the warm pool instances.
--
-- 'autoScalingGroupName', 'deleteWarmPool_autoScalingGroupName' - The name of the Auto Scaling group.
newDeleteWarmPool ::
  -- | 'autoScalingGroupName'
  Prelude.Text ->
  DeleteWarmPool
newDeleteWarmPool pAutoScalingGroupName_ =
  DeleteWarmPool'
    { forceDelete = Prelude.Nothing,
      autoScalingGroupName = pAutoScalingGroupName_
    }

-- | Specifies that the warm pool is to be deleted along with all of its
-- associated instances, without waiting for all instances to be
-- terminated. This parameter also deletes any outstanding lifecycle
-- actions associated with the warm pool instances.
deleteWarmPool_forceDelete :: Lens.Lens' DeleteWarmPool (Prelude.Maybe Prelude.Bool)
deleteWarmPool_forceDelete = Lens.lens (\DeleteWarmPool' {forceDelete} -> forceDelete) (\s@DeleteWarmPool' {} a -> s {forceDelete = a} :: DeleteWarmPool)

-- | The name of the Auto Scaling group.
deleteWarmPool_autoScalingGroupName :: Lens.Lens' DeleteWarmPool Prelude.Text
deleteWarmPool_autoScalingGroupName = Lens.lens (\DeleteWarmPool' {autoScalingGroupName} -> autoScalingGroupName) (\s@DeleteWarmPool' {} a -> s {autoScalingGroupName = a} :: DeleteWarmPool)

instance Core.AWSRequest DeleteWarmPool where
  type
    AWSResponse DeleteWarmPool =
      DeleteWarmPoolResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DeleteWarmPoolResult"
      ( \s h x ->
          DeleteWarmPoolResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteWarmPool where
  hashWithSalt _salt DeleteWarmPool' {..} =
    _salt
      `Prelude.hashWithSalt` forceDelete
      `Prelude.hashWithSalt` autoScalingGroupName

instance Prelude.NFData DeleteWarmPool where
  rnf DeleteWarmPool' {..} =
    Prelude.rnf forceDelete `Prelude.seq`
      Prelude.rnf autoScalingGroupName

instance Data.ToHeaders DeleteWarmPool where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteWarmPool where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteWarmPool where
  toQuery DeleteWarmPool' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteWarmPool" :: Prelude.ByteString),
        "Version"
          Data.=: ("2011-01-01" :: Prelude.ByteString),
        "ForceDelete" Data.=: forceDelete,
        "AutoScalingGroupName" Data.=: autoScalingGroupName
      ]

-- | /See:/ 'newDeleteWarmPoolResponse' smart constructor.
data DeleteWarmPoolResponse = DeleteWarmPoolResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteWarmPoolResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteWarmPoolResponse_httpStatus' - The response's http status code.
newDeleteWarmPoolResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteWarmPoolResponse
newDeleteWarmPoolResponse pHttpStatus_ =
  DeleteWarmPoolResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteWarmPoolResponse_httpStatus :: Lens.Lens' DeleteWarmPoolResponse Prelude.Int
deleteWarmPoolResponse_httpStatus = Lens.lens (\DeleteWarmPoolResponse' {httpStatus} -> httpStatus) (\s@DeleteWarmPoolResponse' {} a -> s {httpStatus = a} :: DeleteWarmPoolResponse)

instance Prelude.NFData DeleteWarmPoolResponse where
  rnf DeleteWarmPoolResponse' {..} =
    Prelude.rnf httpStatus
