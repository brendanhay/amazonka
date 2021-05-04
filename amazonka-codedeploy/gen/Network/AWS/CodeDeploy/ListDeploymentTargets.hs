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
-- Module      : Network.AWS.CodeDeploy.ListDeploymentTargets
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of target IDs that are associated a deployment.
--
-- This operation returns paginated results.
module Network.AWS.CodeDeploy.ListDeploymentTargets
  ( -- * Creating a Request
    ListDeploymentTargets (..),
    newListDeploymentTargets,

    -- * Request Lenses
    listDeploymentTargets_deploymentId,
    listDeploymentTargets_nextToken,
    listDeploymentTargets_targetFilters,

    -- * Destructuring the Response
    ListDeploymentTargetsResponse (..),
    newListDeploymentTargetsResponse,

    -- * Response Lenses
    listDeploymentTargetsResponse_targetIds,
    listDeploymentTargetsResponse_nextToken,
    listDeploymentTargetsResponse_httpStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListDeploymentTargets' smart constructor.
data ListDeploymentTargets = ListDeploymentTargets'
  { -- | The unique ID of a deployment.
    deploymentId :: Prelude.Maybe Prelude.Text,
    -- | A token identifier returned from the previous @ListDeploymentTargets@
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
    targetFilters :: Prelude.Maybe (Prelude.HashMap TargetFilterName [Prelude.Text])
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListDeploymentTargets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentId', 'listDeploymentTargets_deploymentId' - The unique ID of a deployment.
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
newListDeploymentTargets ::
  ListDeploymentTargets
newListDeploymentTargets =
  ListDeploymentTargets'
    { deploymentId =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      targetFilters = Prelude.Nothing
    }

-- | The unique ID of a deployment.
listDeploymentTargets_deploymentId :: Lens.Lens' ListDeploymentTargets (Prelude.Maybe Prelude.Text)
listDeploymentTargets_deploymentId = Lens.lens (\ListDeploymentTargets' {deploymentId} -> deploymentId) (\s@ListDeploymentTargets' {} a -> s {deploymentId = a} :: ListDeploymentTargets)

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
listDeploymentTargets_targetFilters = Lens.lens (\ListDeploymentTargets' {targetFilters} -> targetFilters) (\s@ListDeploymentTargets' {} a -> s {targetFilters = a} :: ListDeploymentTargets) Prelude.. Lens.mapping Prelude._Coerce

instance Pager.AWSPager ListDeploymentTargets where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listDeploymentTargetsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listDeploymentTargetsResponse_targetIds
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listDeploymentTargets_nextToken
          Lens..~ rs
          Lens.^? listDeploymentTargetsResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest ListDeploymentTargets where
  type
    Rs ListDeploymentTargets =
      ListDeploymentTargetsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDeploymentTargetsResponse'
            Prelude.<$> ( x Prelude..?> "targetIds"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (x Prelude..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDeploymentTargets

instance Prelude.NFData ListDeploymentTargets

instance Prelude.ToHeaders ListDeploymentTargets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodeDeploy_20141006.ListDeploymentTargets" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListDeploymentTargets where
  toJSON ListDeploymentTargets' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("deploymentId" Prelude..=)
              Prelude.<$> deploymentId,
            ("nextToken" Prelude..=) Prelude.<$> nextToken,
            ("targetFilters" Prelude..=)
              Prelude.<$> targetFilters
          ]
      )

instance Prelude.ToPath ListDeploymentTargets where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListDeploymentTargets where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListDeploymentTargetsResponse' smart constructor.
data ListDeploymentTargetsResponse = ListDeploymentTargetsResponse'
  { -- | The unique IDs of deployment targets.
    targetIds :: Prelude.Maybe [Prelude.Text],
    -- | If a large amount of information is returned, a token identifier is also
    -- returned. It can be used in a subsequent @ListDeploymentTargets@ call to
    -- return the next set of deployment targets in the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListDeploymentTargetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetIds', 'listDeploymentTargetsResponse_targetIds' - The unique IDs of deployment targets.
--
-- 'nextToken', 'listDeploymentTargetsResponse_nextToken' - If a large amount of information is returned, a token identifier is also
-- returned. It can be used in a subsequent @ListDeploymentTargets@ call to
-- return the next set of deployment targets in the list.
--
-- 'httpStatus', 'listDeploymentTargetsResponse_httpStatus' - The response's http status code.
newListDeploymentTargetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDeploymentTargetsResponse
newListDeploymentTargetsResponse pHttpStatus_ =
  ListDeploymentTargetsResponse'
    { targetIds =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique IDs of deployment targets.
listDeploymentTargetsResponse_targetIds :: Lens.Lens' ListDeploymentTargetsResponse (Prelude.Maybe [Prelude.Text])
listDeploymentTargetsResponse_targetIds = Lens.lens (\ListDeploymentTargetsResponse' {targetIds} -> targetIds) (\s@ListDeploymentTargetsResponse' {} a -> s {targetIds = a} :: ListDeploymentTargetsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | If a large amount of information is returned, a token identifier is also
-- returned. It can be used in a subsequent @ListDeploymentTargets@ call to
-- return the next set of deployment targets in the list.
listDeploymentTargetsResponse_nextToken :: Lens.Lens' ListDeploymentTargetsResponse (Prelude.Maybe Prelude.Text)
listDeploymentTargetsResponse_nextToken = Lens.lens (\ListDeploymentTargetsResponse' {nextToken} -> nextToken) (\s@ListDeploymentTargetsResponse' {} a -> s {nextToken = a} :: ListDeploymentTargetsResponse)

-- | The response's http status code.
listDeploymentTargetsResponse_httpStatus :: Lens.Lens' ListDeploymentTargetsResponse Prelude.Int
listDeploymentTargetsResponse_httpStatus = Lens.lens (\ListDeploymentTargetsResponse' {httpStatus} -> httpStatus) (\s@ListDeploymentTargetsResponse' {} a -> s {httpStatus = a} :: ListDeploymentTargetsResponse)

instance Prelude.NFData ListDeploymentTargetsResponse
