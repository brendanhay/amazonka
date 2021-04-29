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
-- Module      : Network.AWS.CodeDeploy.BatchGetOnPremisesInstances
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about one or more on-premises instances. The maximum
-- number of on-premises instances that can be returned is 25.
module Network.AWS.CodeDeploy.BatchGetOnPremisesInstances
  ( -- * Creating a Request
    BatchGetOnPremisesInstances (..),
    newBatchGetOnPremisesInstances,

    -- * Request Lenses
    batchGetOnPremisesInstances_instanceNames,

    -- * Destructuring the Response
    BatchGetOnPremisesInstancesResponse (..),
    newBatchGetOnPremisesInstancesResponse,

    -- * Response Lenses
    batchGetOnPremisesInstancesResponse_instanceInfos,
    batchGetOnPremisesInstancesResponse_httpStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @BatchGetOnPremisesInstances@ operation.
--
-- /See:/ 'newBatchGetOnPremisesInstances' smart constructor.
data BatchGetOnPremisesInstances = BatchGetOnPremisesInstances'
  { -- | The names of the on-premises instances about which to get information.
    -- The maximum number of instance names you can specify is 25.
    instanceNames :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BatchGetOnPremisesInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceNames', 'batchGetOnPremisesInstances_instanceNames' - The names of the on-premises instances about which to get information.
-- The maximum number of instance names you can specify is 25.
newBatchGetOnPremisesInstances ::
  BatchGetOnPremisesInstances
newBatchGetOnPremisesInstances =
  BatchGetOnPremisesInstances'
    { instanceNames =
        Prelude.mempty
    }

-- | The names of the on-premises instances about which to get information.
-- The maximum number of instance names you can specify is 25.
batchGetOnPremisesInstances_instanceNames :: Lens.Lens' BatchGetOnPremisesInstances [Prelude.Text]
batchGetOnPremisesInstances_instanceNames = Lens.lens (\BatchGetOnPremisesInstances' {instanceNames} -> instanceNames) (\s@BatchGetOnPremisesInstances' {} a -> s {instanceNames = a} :: BatchGetOnPremisesInstances) Prelude.. Prelude._Coerce

instance
  Prelude.AWSRequest
    BatchGetOnPremisesInstances
  where
  type
    Rs BatchGetOnPremisesInstances =
      BatchGetOnPremisesInstancesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetOnPremisesInstancesResponse'
            Prelude.<$> ( x Prelude..?> "instanceInfos"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchGetOnPremisesInstances

instance Prelude.NFData BatchGetOnPremisesInstances

instance
  Prelude.ToHeaders
    BatchGetOnPremisesInstances
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodeDeploy_20141006.BatchGetOnPremisesInstances" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON BatchGetOnPremisesInstances where
  toJSON BatchGetOnPremisesInstances' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("instanceNames" Prelude..= instanceNames)
          ]
      )

instance Prelude.ToPath BatchGetOnPremisesInstances where
  toPath = Prelude.const "/"

instance Prelude.ToQuery BatchGetOnPremisesInstances where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @BatchGetOnPremisesInstances@ operation.
--
-- /See:/ 'newBatchGetOnPremisesInstancesResponse' smart constructor.
data BatchGetOnPremisesInstancesResponse = BatchGetOnPremisesInstancesResponse'
  { -- | Information about the on-premises instances.
    instanceInfos :: Prelude.Maybe [InstanceInfo],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BatchGetOnPremisesInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceInfos', 'batchGetOnPremisesInstancesResponse_instanceInfos' - Information about the on-premises instances.
--
-- 'httpStatus', 'batchGetOnPremisesInstancesResponse_httpStatus' - The response's http status code.
newBatchGetOnPremisesInstancesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchGetOnPremisesInstancesResponse
newBatchGetOnPremisesInstancesResponse pHttpStatus_ =
  BatchGetOnPremisesInstancesResponse'
    { instanceInfos =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the on-premises instances.
batchGetOnPremisesInstancesResponse_instanceInfos :: Lens.Lens' BatchGetOnPremisesInstancesResponse (Prelude.Maybe [InstanceInfo])
batchGetOnPremisesInstancesResponse_instanceInfos = Lens.lens (\BatchGetOnPremisesInstancesResponse' {instanceInfos} -> instanceInfos) (\s@BatchGetOnPremisesInstancesResponse' {} a -> s {instanceInfos = a} :: BatchGetOnPremisesInstancesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
batchGetOnPremisesInstancesResponse_httpStatus :: Lens.Lens' BatchGetOnPremisesInstancesResponse Prelude.Int
batchGetOnPremisesInstancesResponse_httpStatus = Lens.lens (\BatchGetOnPremisesInstancesResponse' {httpStatus} -> httpStatus) (\s@BatchGetOnPremisesInstancesResponse' {} a -> s {httpStatus = a} :: BatchGetOnPremisesInstancesResponse)

instance
  Prelude.NFData
    BatchGetOnPremisesInstancesResponse
