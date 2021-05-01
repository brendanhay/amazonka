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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @ListOnPremisesInstances@ operation.
--
-- /See:/ 'newListOnPremisesInstances' smart constructor.
data ListOnPremisesInstances = ListOnPremisesInstances'
  { -- | An identifier returned from the previous list on-premises instances
    -- call. It can be used to return the next set of on-premises instances in
    -- the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The on-premises instance tags that are used to restrict the on-premises
    -- instance names returned.
    tagFilters :: Prelude.Maybe [TagFilter],
    -- | The registration status of the on-premises instances:
    --
    -- -   @Deregistered@: Include deregistered on-premises instances in the
    --     resulting list.
    --
    -- -   @Registered@: Include registered on-premises instances in the
    --     resulting list.
    registrationStatus :: Prelude.Maybe RegistrationStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { nextToken =
        Prelude.Nothing,
      tagFilters = Prelude.Nothing,
      registrationStatus = Prelude.Nothing
    }

-- | An identifier returned from the previous list on-premises instances
-- call. It can be used to return the next set of on-premises instances in
-- the list.
listOnPremisesInstances_nextToken :: Lens.Lens' ListOnPremisesInstances (Prelude.Maybe Prelude.Text)
listOnPremisesInstances_nextToken = Lens.lens (\ListOnPremisesInstances' {nextToken} -> nextToken) (\s@ListOnPremisesInstances' {} a -> s {nextToken = a} :: ListOnPremisesInstances)

-- | The on-premises instance tags that are used to restrict the on-premises
-- instance names returned.
listOnPremisesInstances_tagFilters :: Lens.Lens' ListOnPremisesInstances (Prelude.Maybe [TagFilter])
listOnPremisesInstances_tagFilters = Lens.lens (\ListOnPremisesInstances' {tagFilters} -> tagFilters) (\s@ListOnPremisesInstances' {} a -> s {tagFilters = a} :: ListOnPremisesInstances) Prelude.. Lens.mapping Prelude._Coerce

-- | The registration status of the on-premises instances:
--
-- -   @Deregistered@: Include deregistered on-premises instances in the
--     resulting list.
--
-- -   @Registered@: Include registered on-premises instances in the
--     resulting list.
listOnPremisesInstances_registrationStatus :: Lens.Lens' ListOnPremisesInstances (Prelude.Maybe RegistrationStatus)
listOnPremisesInstances_registrationStatus = Lens.lens (\ListOnPremisesInstances' {registrationStatus} -> registrationStatus) (\s@ListOnPremisesInstances' {} a -> s {registrationStatus = a} :: ListOnPremisesInstances)

instance Pager.AWSPager ListOnPremisesInstances where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listOnPremisesInstancesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listOnPremisesInstancesResponse_instanceNames
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listOnPremisesInstances_nextToken
          Lens..~ rs
          Lens.^? listOnPremisesInstancesResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest ListOnPremisesInstances where
  type
    Rs ListOnPremisesInstances =
      ListOnPremisesInstancesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListOnPremisesInstancesResponse'
            Prelude.<$> (x Prelude..?> "nextToken")
            Prelude.<*> ( x Prelude..?> "instanceNames"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListOnPremisesInstances

instance Prelude.NFData ListOnPremisesInstances

instance Prelude.ToHeaders ListOnPremisesInstances where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodeDeploy_20141006.ListOnPremisesInstances" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListOnPremisesInstances where
  toJSON ListOnPremisesInstances' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("nextToken" Prelude..=) Prelude.<$> nextToken,
            ("tagFilters" Prelude..=) Prelude.<$> tagFilters,
            ("registrationStatus" Prelude..=)
              Prelude.<$> registrationStatus
          ]
      )

instance Prelude.ToPath ListOnPremisesInstances where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListOnPremisesInstances where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of the list on-premises instances operation.
--
-- /See:/ 'newListOnPremisesInstancesResponse' smart constructor.
data ListOnPremisesInstancesResponse = ListOnPremisesInstancesResponse'
  { -- | If a large amount of information is returned, an identifier is also
    -- returned. It can be used in a subsequent list on-premises instances call
    -- to return the next set of on-premises instances in the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of matching on-premises instance names.
    instanceNames :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  ListOnPremisesInstancesResponse
newListOnPremisesInstancesResponse pHttpStatus_ =
  ListOnPremisesInstancesResponse'
    { nextToken =
        Prelude.Nothing,
      instanceNames = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If a large amount of information is returned, an identifier is also
-- returned. It can be used in a subsequent list on-premises instances call
-- to return the next set of on-premises instances in the list.
listOnPremisesInstancesResponse_nextToken :: Lens.Lens' ListOnPremisesInstancesResponse (Prelude.Maybe Prelude.Text)
listOnPremisesInstancesResponse_nextToken = Lens.lens (\ListOnPremisesInstancesResponse' {nextToken} -> nextToken) (\s@ListOnPremisesInstancesResponse' {} a -> s {nextToken = a} :: ListOnPremisesInstancesResponse)

-- | The list of matching on-premises instance names.
listOnPremisesInstancesResponse_instanceNames :: Lens.Lens' ListOnPremisesInstancesResponse (Prelude.Maybe [Prelude.Text])
listOnPremisesInstancesResponse_instanceNames = Lens.lens (\ListOnPremisesInstancesResponse' {instanceNames} -> instanceNames) (\s@ListOnPremisesInstancesResponse' {} a -> s {instanceNames = a} :: ListOnPremisesInstancesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listOnPremisesInstancesResponse_httpStatus :: Lens.Lens' ListOnPremisesInstancesResponse Prelude.Int
listOnPremisesInstancesResponse_httpStatus = Lens.lens (\ListOnPremisesInstancesResponse' {httpStatus} -> httpStatus) (\s@ListOnPremisesInstancesResponse' {} a -> s {httpStatus = a} :: ListOnPremisesInstancesResponse)

instance
  Prelude.NFData
    ListOnPremisesInstancesResponse
