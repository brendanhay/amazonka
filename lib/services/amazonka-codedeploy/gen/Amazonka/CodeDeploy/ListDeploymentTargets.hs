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
-- Module      : Amazonka.CodeDeploy.ListDeploymentTargets
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of target IDs that are associated a deployment.
--
-- This operation returns paginated results.
module Amazonka.CodeDeploy.ListDeploymentTargets
  ( -- * Creating a Request
    ListDeploymentTargets (..),
    newListDeploymentTargets,

    -- * Request Lenses
    listDeploymentTargets_nextToken,
    listDeploymentTargets_targetFilters,
    listDeploymentTargets_deploymentId,

    -- * Destructuring the Response
    ListDeploymentTargetsResponse (..),
    newListDeploymentTargetsResponse,

    -- * Response Lenses
    listDeploymentTargetsResponse_nextToken,
    listDeploymentTargetsResponse_targetIds,
    listDeploymentTargetsResponse_httpStatus,
  )
where

import Amazonka.CodeDeploy.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDeploymentTargets' smart constructor.
data ListDeploymentTargets = ListDeploymentTargets'
  { -- | A token identifier returned from the previous @ListDeploymentTargets@
    -- call. It can be used to return the next set of deployment targets in the
    -- list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A key used to filter the returned targets. The two valid values are:
    --
    -- -   @TargetStatus@ - A @TargetStatus@ filter string can be @Failed@,
    --     @InProgress@, @Pending@, @Ready@, @Skipped@, @Succeeded@, or
    --     @Unknown@.
    --
    -- -   @ServerInstanceLabel@ - A @ServerInstanceLabel@ filter string can be
    --     @Blue@ or @Green@.
    targetFilters :: Prelude.Maybe (Prelude.HashMap TargetFilterName [Prelude.Text]),
    -- | The unique ID of a deployment.
    deploymentId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDeploymentTargets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDeploymentTargets_nextToken' - A token identifier returned from the previous @ListDeploymentTargets@
-- call. It can be used to return the next set of deployment targets in the
-- list.
--
-- 'targetFilters', 'listDeploymentTargets_targetFilters' - A key used to filter the returned targets. The two valid values are:
--
-- -   @TargetStatus@ - A @TargetStatus@ filter string can be @Failed@,
--     @InProgress@, @Pending@, @Ready@, @Skipped@, @Succeeded@, or
--     @Unknown@.
--
-- -   @ServerInstanceLabel@ - A @ServerInstanceLabel@ filter string can be
--     @Blue@ or @Green@.
--
-- 'deploymentId', 'listDeploymentTargets_deploymentId' - The unique ID of a deployment.
newListDeploymentTargets ::
  ListDeploymentTargets
newListDeploymentTargets =
  ListDeploymentTargets'
    { nextToken = Prelude.Nothing,
      targetFilters = Prelude.Nothing,
      deploymentId = Prelude.Nothing
    }

-- | A token identifier returned from the previous @ListDeploymentTargets@
-- call. It can be used to return the next set of deployment targets in the
-- list.
listDeploymentTargets_nextToken :: Lens.Lens' ListDeploymentTargets (Prelude.Maybe Prelude.Text)
listDeploymentTargets_nextToken = Lens.lens (\ListDeploymentTargets' {nextToken} -> nextToken) (\s@ListDeploymentTargets' {} a -> s {nextToken = a} :: ListDeploymentTargets)

-- | A key used to filter the returned targets. The two valid values are:
--
-- -   @TargetStatus@ - A @TargetStatus@ filter string can be @Failed@,
--     @InProgress@, @Pending@, @Ready@, @Skipped@, @Succeeded@, or
--     @Unknown@.
--
-- -   @ServerInstanceLabel@ - A @ServerInstanceLabel@ filter string can be
--     @Blue@ or @Green@.
listDeploymentTargets_targetFilters :: Lens.Lens' ListDeploymentTargets (Prelude.Maybe (Prelude.HashMap TargetFilterName [Prelude.Text]))
listDeploymentTargets_targetFilters = Lens.lens (\ListDeploymentTargets' {targetFilters} -> targetFilters) (\s@ListDeploymentTargets' {} a -> s {targetFilters = a} :: ListDeploymentTargets) Prelude.. Lens.mapping Lens.coerced

-- | The unique ID of a deployment.
listDeploymentTargets_deploymentId :: Lens.Lens' ListDeploymentTargets (Prelude.Maybe Prelude.Text)
listDeploymentTargets_deploymentId = Lens.lens (\ListDeploymentTargets' {deploymentId} -> deploymentId) (\s@ListDeploymentTargets' {} a -> s {deploymentId = a} :: ListDeploymentTargets)

instance Core.AWSPager ListDeploymentTargets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDeploymentTargetsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listDeploymentTargetsResponse_targetIds
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listDeploymentTargets_nextToken
          Lens..~ rs
          Lens.^? listDeploymentTargetsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListDeploymentTargets where
  type
    AWSResponse ListDeploymentTargets =
      ListDeploymentTargetsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDeploymentTargetsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "targetIds" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDeploymentTargets where
  hashWithSalt _salt ListDeploymentTargets' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` targetFilters
      `Prelude.hashWithSalt` deploymentId

instance Prelude.NFData ListDeploymentTargets where
  rnf ListDeploymentTargets' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf targetFilters
      `Prelude.seq` Prelude.rnf deploymentId

instance Data.ToHeaders ListDeploymentTargets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeDeploy_20141006.ListDeploymentTargets" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListDeploymentTargets where
  toJSON ListDeploymentTargets' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("nextToken" Data..=) Prelude.<$> nextToken,
            ("targetFilters" Data..=) Prelude.<$> targetFilters,
            ("deploymentId" Data..=) Prelude.<$> deploymentId
          ]
      )

instance Data.ToPath ListDeploymentTargets where
  toPath = Prelude.const "/"

instance Data.ToQuery ListDeploymentTargets where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListDeploymentTargetsResponse' smart constructor.
data ListDeploymentTargetsResponse = ListDeploymentTargetsResponse'
  { -- | If a large amount of information is returned, a token identifier is also
    -- returned. It can be used in a subsequent @ListDeploymentTargets@ call to
    -- return the next set of deployment targets in the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The unique IDs of deployment targets.
    targetIds :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDeploymentTargetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDeploymentTargetsResponse_nextToken' - If a large amount of information is returned, a token identifier is also
-- returned. It can be used in a subsequent @ListDeploymentTargets@ call to
-- return the next set of deployment targets in the list.
--
-- 'targetIds', 'listDeploymentTargetsResponse_targetIds' - The unique IDs of deployment targets.
--
-- 'httpStatus', 'listDeploymentTargetsResponse_httpStatus' - The response's http status code.
newListDeploymentTargetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDeploymentTargetsResponse
newListDeploymentTargetsResponse pHttpStatus_ =
  ListDeploymentTargetsResponse'
    { nextToken =
        Prelude.Nothing,
      targetIds = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If a large amount of information is returned, a token identifier is also
-- returned. It can be used in a subsequent @ListDeploymentTargets@ call to
-- return the next set of deployment targets in the list.
listDeploymentTargetsResponse_nextToken :: Lens.Lens' ListDeploymentTargetsResponse (Prelude.Maybe Prelude.Text)
listDeploymentTargetsResponse_nextToken = Lens.lens (\ListDeploymentTargetsResponse' {nextToken} -> nextToken) (\s@ListDeploymentTargetsResponse' {} a -> s {nextToken = a} :: ListDeploymentTargetsResponse)

-- | The unique IDs of deployment targets.
listDeploymentTargetsResponse_targetIds :: Lens.Lens' ListDeploymentTargetsResponse (Prelude.Maybe [Prelude.Text])
listDeploymentTargetsResponse_targetIds = Lens.lens (\ListDeploymentTargetsResponse' {targetIds} -> targetIds) (\s@ListDeploymentTargetsResponse' {} a -> s {targetIds = a} :: ListDeploymentTargetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listDeploymentTargetsResponse_httpStatus :: Lens.Lens' ListDeploymentTargetsResponse Prelude.Int
listDeploymentTargetsResponse_httpStatus = Lens.lens (\ListDeploymentTargetsResponse' {httpStatus} -> httpStatus) (\s@ListDeploymentTargetsResponse' {} a -> s {httpStatus = a} :: ListDeploymentTargetsResponse)

instance Prelude.NFData ListDeploymentTargetsResponse where
  rnf ListDeploymentTargetsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf targetIds
      `Prelude.seq` Prelude.rnf httpStatus
