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
-- Module      : Network.AWS.CodeDeploy.ListOnPremisesInstances
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of names for one or more on-premises instances.
--
-- Unless otherwise specified, both registered and deregistered on-premises
-- instance names are listed. To list only registered or deregistered
-- on-premises instance names, use the registration status parameter.
--
-- This operation returns paginated results.
module Network.AWS.CodeDeploy.ListOnPremisesInstances
  ( -- * Creating a Request
    ListOnPremisesInstances (..),
    newListOnPremisesInstances,

    -- * Request Lenses
    listOnPremisesInstances_nextToken,
    listOnPremisesInstances_tagFilters,
    listOnPremisesInstances_registrationStatus,

    -- * Destructuring the Response
    ListOnPremisesInstancesResponse (..),
    newListOnPremisesInstancesResponse,

    -- * Response Lenses
    listOnPremisesInstancesResponse_nextToken,
    listOnPremisesInstancesResponse_instanceNames,
    listOnPremisesInstancesResponse_httpStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @ListOnPremisesInstances@ operation.
--
-- /See:/ 'newListOnPremisesInstances' smart constructor.
data ListOnPremisesInstances = ListOnPremisesInstances'
  { -- | An identifier returned from the previous list on-premises instances
    -- call. It can be used to return the next set of on-premises instances in
    -- the list.
    nextToken :: Core.Maybe Core.Text,
    -- | The on-premises instance tags that are used to restrict the on-premises
    -- instance names returned.
    tagFilters :: Core.Maybe [TagFilter],
    -- | The registration status of the on-premises instances:
    --
    -- -   @Deregistered@: Include deregistered on-premises instances in the
    --     resulting list.
    --
    -- -   @Registered@: Include registered on-premises instances in the
    --     resulting list.
    registrationStatus :: Core.Maybe RegistrationStatus
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListOnPremisesInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listOnPremisesInstances_nextToken' - An identifier returned from the previous list on-premises instances
-- call. It can be used to return the next set of on-premises instances in
-- the list.
--
-- 'tagFilters', 'listOnPremisesInstances_tagFilters' - The on-premises instance tags that are used to restrict the on-premises
-- instance names returned.
--
-- 'registrationStatus', 'listOnPremisesInstances_registrationStatus' - The registration status of the on-premises instances:
--
-- -   @Deregistered@: Include deregistered on-premises instances in the
--     resulting list.
--
-- -   @Registered@: Include registered on-premises instances in the
--     resulting list.
newListOnPremisesInstances ::
  ListOnPremisesInstances
newListOnPremisesInstances =
  ListOnPremisesInstances'
    { nextToken = Core.Nothing,
      tagFilters = Core.Nothing,
      registrationStatus = Core.Nothing
    }

-- | An identifier returned from the previous list on-premises instances
-- call. It can be used to return the next set of on-premises instances in
-- the list.
listOnPremisesInstances_nextToken :: Lens.Lens' ListOnPremisesInstances (Core.Maybe Core.Text)
listOnPremisesInstances_nextToken = Lens.lens (\ListOnPremisesInstances' {nextToken} -> nextToken) (\s@ListOnPremisesInstances' {} a -> s {nextToken = a} :: ListOnPremisesInstances)

-- | The on-premises instance tags that are used to restrict the on-premises
-- instance names returned.
listOnPremisesInstances_tagFilters :: Lens.Lens' ListOnPremisesInstances (Core.Maybe [TagFilter])
listOnPremisesInstances_tagFilters = Lens.lens (\ListOnPremisesInstances' {tagFilters} -> tagFilters) (\s@ListOnPremisesInstances' {} a -> s {tagFilters = a} :: ListOnPremisesInstances) Core.. Lens.mapping Lens._Coerce

-- | The registration status of the on-premises instances:
--
-- -   @Deregistered@: Include deregistered on-premises instances in the
--     resulting list.
--
-- -   @Registered@: Include registered on-premises instances in the
--     resulting list.
listOnPremisesInstances_registrationStatus :: Lens.Lens' ListOnPremisesInstances (Core.Maybe RegistrationStatus)
listOnPremisesInstances_registrationStatus = Lens.lens (\ListOnPremisesInstances' {registrationStatus} -> registrationStatus) (\s@ListOnPremisesInstances' {} a -> s {registrationStatus = a} :: ListOnPremisesInstances)

instance Core.AWSPager ListOnPremisesInstances where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listOnPremisesInstancesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listOnPremisesInstancesResponse_instanceNames
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listOnPremisesInstances_nextToken
          Lens..~ rs
          Lens.^? listOnPremisesInstancesResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListOnPremisesInstances where
  type
    AWSResponse ListOnPremisesInstances =
      ListOnPremisesInstancesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListOnPremisesInstancesResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "instanceNames" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListOnPremisesInstances

instance Core.NFData ListOnPremisesInstances

instance Core.ToHeaders ListOnPremisesInstances where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeDeploy_20141006.ListOnPremisesInstances" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListOnPremisesInstances where
  toJSON ListOnPremisesInstances' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("tagFilters" Core..=) Core.<$> tagFilters,
            ("registrationStatus" Core..=)
              Core.<$> registrationStatus
          ]
      )

instance Core.ToPath ListOnPremisesInstances where
  toPath = Core.const "/"

instance Core.ToQuery ListOnPremisesInstances where
  toQuery = Core.const Core.mempty

-- | Represents the output of the list on-premises instances operation.
--
-- /See:/ 'newListOnPremisesInstancesResponse' smart constructor.
data ListOnPremisesInstancesResponse = ListOnPremisesInstancesResponse'
  { -- | If a large amount of information is returned, an identifier is also
    -- returned. It can be used in a subsequent list on-premises instances call
    -- to return the next set of on-premises instances in the list.
    nextToken :: Core.Maybe Core.Text,
    -- | The list of matching on-premises instance names.
    instanceNames :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListOnPremisesInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listOnPremisesInstancesResponse_nextToken' - If a large amount of information is returned, an identifier is also
-- returned. It can be used in a subsequent list on-premises instances call
-- to return the next set of on-premises instances in the list.
--
-- 'instanceNames', 'listOnPremisesInstancesResponse_instanceNames' - The list of matching on-premises instance names.
--
-- 'httpStatus', 'listOnPremisesInstancesResponse_httpStatus' - The response's http status code.
newListOnPremisesInstancesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListOnPremisesInstancesResponse
newListOnPremisesInstancesResponse pHttpStatus_ =
  ListOnPremisesInstancesResponse'
    { nextToken =
        Core.Nothing,
      instanceNames = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If a large amount of information is returned, an identifier is also
-- returned. It can be used in a subsequent list on-premises instances call
-- to return the next set of on-premises instances in the list.
listOnPremisesInstancesResponse_nextToken :: Lens.Lens' ListOnPremisesInstancesResponse (Core.Maybe Core.Text)
listOnPremisesInstancesResponse_nextToken = Lens.lens (\ListOnPremisesInstancesResponse' {nextToken} -> nextToken) (\s@ListOnPremisesInstancesResponse' {} a -> s {nextToken = a} :: ListOnPremisesInstancesResponse)

-- | The list of matching on-premises instance names.
listOnPremisesInstancesResponse_instanceNames :: Lens.Lens' ListOnPremisesInstancesResponse (Core.Maybe [Core.Text])
listOnPremisesInstancesResponse_instanceNames = Lens.lens (\ListOnPremisesInstancesResponse' {instanceNames} -> instanceNames) (\s@ListOnPremisesInstancesResponse' {} a -> s {instanceNames = a} :: ListOnPremisesInstancesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listOnPremisesInstancesResponse_httpStatus :: Lens.Lens' ListOnPremisesInstancesResponse Core.Int
listOnPremisesInstancesResponse_httpStatus = Lens.lens (\ListOnPremisesInstancesResponse' {httpStatus} -> httpStatus) (\s@ListOnPremisesInstancesResponse' {} a -> s {httpStatus = a} :: ListOnPremisesInstancesResponse)

instance Core.NFData ListOnPremisesInstancesResponse
