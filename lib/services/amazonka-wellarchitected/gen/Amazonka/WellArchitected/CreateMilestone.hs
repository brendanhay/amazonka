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
-- Module      : Amazonka.WellArchitected.CreateMilestone
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a milestone for an existing workload.
module Amazonka.WellArchitected.CreateMilestone
  ( -- * Creating a Request
    CreateMilestone (..),
    newCreateMilestone,

    -- * Request Lenses
    createMilestone_workloadId,
    createMilestone_milestoneName,
    createMilestone_clientRequestToken,

    -- * Destructuring the Response
    CreateMilestoneResponse (..),
    newCreateMilestoneResponse,

    -- * Response Lenses
    createMilestoneResponse_milestoneNumber,
    createMilestoneResponse_workloadId,
    createMilestoneResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WellArchitected.Types

-- | Input for milestone creation.
--
-- /See:/ 'newCreateMilestone' smart constructor.
data CreateMilestone = CreateMilestone'
  { workloadId :: Prelude.Text,
    milestoneName :: Prelude.Text,
    clientRequestToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMilestone' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workloadId', 'createMilestone_workloadId' - Undocumented member.
--
-- 'milestoneName', 'createMilestone_milestoneName' - Undocumented member.
--
-- 'clientRequestToken', 'createMilestone_clientRequestToken' - Undocumented member.
newCreateMilestone ::
  -- | 'workloadId'
  Prelude.Text ->
  -- | 'milestoneName'
  Prelude.Text ->
  -- | 'clientRequestToken'
  Prelude.Text ->
  CreateMilestone
newCreateMilestone
  pWorkloadId_
  pMilestoneName_
  pClientRequestToken_ =
    CreateMilestone'
      { workloadId = pWorkloadId_,
        milestoneName = pMilestoneName_,
        clientRequestToken = pClientRequestToken_
      }

-- | Undocumented member.
createMilestone_workloadId :: Lens.Lens' CreateMilestone Prelude.Text
createMilestone_workloadId = Lens.lens (\CreateMilestone' {workloadId} -> workloadId) (\s@CreateMilestone' {} a -> s {workloadId = a} :: CreateMilestone)

-- | Undocumented member.
createMilestone_milestoneName :: Lens.Lens' CreateMilestone Prelude.Text
createMilestone_milestoneName = Lens.lens (\CreateMilestone' {milestoneName} -> milestoneName) (\s@CreateMilestone' {} a -> s {milestoneName = a} :: CreateMilestone)

-- | Undocumented member.
createMilestone_clientRequestToken :: Lens.Lens' CreateMilestone Prelude.Text
createMilestone_clientRequestToken = Lens.lens (\CreateMilestone' {clientRequestToken} -> clientRequestToken) (\s@CreateMilestone' {} a -> s {clientRequestToken = a} :: CreateMilestone)

instance Core.AWSRequest CreateMilestone where
  type
    AWSResponse CreateMilestone =
      CreateMilestoneResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateMilestoneResponse'
            Prelude.<$> (x Data..?> "MilestoneNumber")
            Prelude.<*> (x Data..?> "WorkloadId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateMilestone where
  hashWithSalt _salt CreateMilestone' {..} =
    _salt
      `Prelude.hashWithSalt` workloadId
      `Prelude.hashWithSalt` milestoneName
      `Prelude.hashWithSalt` clientRequestToken

instance Prelude.NFData CreateMilestone where
  rnf CreateMilestone' {..} =
    Prelude.rnf workloadId `Prelude.seq`
      Prelude.rnf milestoneName `Prelude.seq`
        Prelude.rnf clientRequestToken

instance Data.ToHeaders CreateMilestone where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateMilestone where
  toJSON CreateMilestone' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("MilestoneName" Data..= milestoneName),
            Prelude.Just
              ("ClientRequestToken" Data..= clientRequestToken)
          ]
      )

instance Data.ToPath CreateMilestone where
  toPath CreateMilestone' {..} =
    Prelude.mconcat
      ["/workloads/", Data.toBS workloadId, "/milestones"]

instance Data.ToQuery CreateMilestone where
  toQuery = Prelude.const Prelude.mempty

-- | Output of a create milestone call.
--
-- /See:/ 'newCreateMilestoneResponse' smart constructor.
data CreateMilestoneResponse = CreateMilestoneResponse'
  { milestoneNumber :: Prelude.Maybe Prelude.Natural,
    workloadId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMilestoneResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'milestoneNumber', 'createMilestoneResponse_milestoneNumber' - Undocumented member.
--
-- 'workloadId', 'createMilestoneResponse_workloadId' - Undocumented member.
--
-- 'httpStatus', 'createMilestoneResponse_httpStatus' - The response's http status code.
newCreateMilestoneResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateMilestoneResponse
newCreateMilestoneResponse pHttpStatus_ =
  CreateMilestoneResponse'
    { milestoneNumber =
        Prelude.Nothing,
      workloadId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createMilestoneResponse_milestoneNumber :: Lens.Lens' CreateMilestoneResponse (Prelude.Maybe Prelude.Natural)
createMilestoneResponse_milestoneNumber = Lens.lens (\CreateMilestoneResponse' {milestoneNumber} -> milestoneNumber) (\s@CreateMilestoneResponse' {} a -> s {milestoneNumber = a} :: CreateMilestoneResponse)

-- | Undocumented member.
createMilestoneResponse_workloadId :: Lens.Lens' CreateMilestoneResponse (Prelude.Maybe Prelude.Text)
createMilestoneResponse_workloadId = Lens.lens (\CreateMilestoneResponse' {workloadId} -> workloadId) (\s@CreateMilestoneResponse' {} a -> s {workloadId = a} :: CreateMilestoneResponse)

-- | The response's http status code.
createMilestoneResponse_httpStatus :: Lens.Lens' CreateMilestoneResponse Prelude.Int
createMilestoneResponse_httpStatus = Lens.lens (\CreateMilestoneResponse' {httpStatus} -> httpStatus) (\s@CreateMilestoneResponse' {} a -> s {httpStatus = a} :: CreateMilestoneResponse)

instance Prelude.NFData CreateMilestoneResponse where
  rnf CreateMilestoneResponse' {..} =
    Prelude.rnf milestoneNumber `Prelude.seq`
      Prelude.rnf workloadId `Prelude.seq`
        Prelude.rnf httpStatus
