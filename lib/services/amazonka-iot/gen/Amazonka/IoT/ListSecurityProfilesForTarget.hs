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
-- Module      : Amazonka.IoT.ListSecurityProfilesForTarget
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the Device Defender security profiles attached to a target (thing
-- group).
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions ListSecurityProfilesForTarget>
-- action.
--
-- This operation returns paginated results.
module Amazonka.IoT.ListSecurityProfilesForTarget
  ( -- * Creating a Request
    ListSecurityProfilesForTarget (..),
    newListSecurityProfilesForTarget,

    -- * Request Lenses
    listSecurityProfilesForTarget_maxResults,
    listSecurityProfilesForTarget_nextToken,
    listSecurityProfilesForTarget_recursive,
    listSecurityProfilesForTarget_securityProfileTargetArn,

    -- * Destructuring the Response
    ListSecurityProfilesForTargetResponse (..),
    newListSecurityProfilesForTargetResponse,

    -- * Response Lenses
    listSecurityProfilesForTargetResponse_nextToken,
    listSecurityProfilesForTargetResponse_securityProfileTargetMappings,
    listSecurityProfilesForTargetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListSecurityProfilesForTarget' smart constructor.
data ListSecurityProfilesForTarget = ListSecurityProfilesForTarget'
  { -- | The maximum number of results to return at one time.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | If true, return child groups too.
    recursive :: Prelude.Maybe Prelude.Bool,
    -- | The ARN of the target (thing group) whose attached security profiles you
    -- want to get.
    securityProfileTargetArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSecurityProfilesForTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listSecurityProfilesForTarget_maxResults' - The maximum number of results to return at one time.
--
-- 'nextToken', 'listSecurityProfilesForTarget_nextToken' - The token for the next set of results.
--
-- 'recursive', 'listSecurityProfilesForTarget_recursive' - If true, return child groups too.
--
-- 'securityProfileTargetArn', 'listSecurityProfilesForTarget_securityProfileTargetArn' - The ARN of the target (thing group) whose attached security profiles you
-- want to get.
newListSecurityProfilesForTarget ::
  -- | 'securityProfileTargetArn'
  Prelude.Text ->
  ListSecurityProfilesForTarget
newListSecurityProfilesForTarget
  pSecurityProfileTargetArn_ =
    ListSecurityProfilesForTarget'
      { maxResults =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        recursive = Prelude.Nothing,
        securityProfileTargetArn =
          pSecurityProfileTargetArn_
      }

-- | The maximum number of results to return at one time.
listSecurityProfilesForTarget_maxResults :: Lens.Lens' ListSecurityProfilesForTarget (Prelude.Maybe Prelude.Natural)
listSecurityProfilesForTarget_maxResults = Lens.lens (\ListSecurityProfilesForTarget' {maxResults} -> maxResults) (\s@ListSecurityProfilesForTarget' {} a -> s {maxResults = a} :: ListSecurityProfilesForTarget)

-- | The token for the next set of results.
listSecurityProfilesForTarget_nextToken :: Lens.Lens' ListSecurityProfilesForTarget (Prelude.Maybe Prelude.Text)
listSecurityProfilesForTarget_nextToken = Lens.lens (\ListSecurityProfilesForTarget' {nextToken} -> nextToken) (\s@ListSecurityProfilesForTarget' {} a -> s {nextToken = a} :: ListSecurityProfilesForTarget)

-- | If true, return child groups too.
listSecurityProfilesForTarget_recursive :: Lens.Lens' ListSecurityProfilesForTarget (Prelude.Maybe Prelude.Bool)
listSecurityProfilesForTarget_recursive = Lens.lens (\ListSecurityProfilesForTarget' {recursive} -> recursive) (\s@ListSecurityProfilesForTarget' {} a -> s {recursive = a} :: ListSecurityProfilesForTarget)

-- | The ARN of the target (thing group) whose attached security profiles you
-- want to get.
listSecurityProfilesForTarget_securityProfileTargetArn :: Lens.Lens' ListSecurityProfilesForTarget Prelude.Text
listSecurityProfilesForTarget_securityProfileTargetArn = Lens.lens (\ListSecurityProfilesForTarget' {securityProfileTargetArn} -> securityProfileTargetArn) (\s@ListSecurityProfilesForTarget' {} a -> s {securityProfileTargetArn = a} :: ListSecurityProfilesForTarget)

instance Core.AWSPager ListSecurityProfilesForTarget where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSecurityProfilesForTargetResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listSecurityProfilesForTargetResponse_securityProfileTargetMappings
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listSecurityProfilesForTarget_nextToken
          Lens..~ rs
          Lens.^? listSecurityProfilesForTargetResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListSecurityProfilesForTarget
  where
  type
    AWSResponse ListSecurityProfilesForTarget =
      ListSecurityProfilesForTargetResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSecurityProfilesForTargetResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> ( x Data..?> "securityProfileTargetMappings"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListSecurityProfilesForTarget
  where
  hashWithSalt _salt ListSecurityProfilesForTarget' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` recursive
      `Prelude.hashWithSalt` securityProfileTargetArn

instance Prelude.NFData ListSecurityProfilesForTarget where
  rnf ListSecurityProfilesForTarget' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf recursive
      `Prelude.seq` Prelude.rnf securityProfileTargetArn

instance Data.ToHeaders ListSecurityProfilesForTarget where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListSecurityProfilesForTarget where
  toPath =
    Prelude.const "/security-profiles-for-target"

instance Data.ToQuery ListSecurityProfilesForTarget where
  toQuery ListSecurityProfilesForTarget' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "recursive" Data.=: recursive,
        "securityProfileTargetArn"
          Data.=: securityProfileTargetArn
      ]

-- | /See:/ 'newListSecurityProfilesForTargetResponse' smart constructor.
data ListSecurityProfilesForTargetResponse = ListSecurityProfilesForTargetResponse'
  { -- | A token that can be used to retrieve the next set of results, or @null@
    -- if there are no additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of security profiles and their associated targets.
    securityProfileTargetMappings :: Prelude.Maybe [SecurityProfileTargetMapping],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSecurityProfilesForTargetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSecurityProfilesForTargetResponse_nextToken' - A token that can be used to retrieve the next set of results, or @null@
-- if there are no additional results.
--
-- 'securityProfileTargetMappings', 'listSecurityProfilesForTargetResponse_securityProfileTargetMappings' - A list of security profiles and their associated targets.
--
-- 'httpStatus', 'listSecurityProfilesForTargetResponse_httpStatus' - The response's http status code.
newListSecurityProfilesForTargetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSecurityProfilesForTargetResponse
newListSecurityProfilesForTargetResponse pHttpStatus_ =
  ListSecurityProfilesForTargetResponse'
    { nextToken =
        Prelude.Nothing,
      securityProfileTargetMappings =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token that can be used to retrieve the next set of results, or @null@
-- if there are no additional results.
listSecurityProfilesForTargetResponse_nextToken :: Lens.Lens' ListSecurityProfilesForTargetResponse (Prelude.Maybe Prelude.Text)
listSecurityProfilesForTargetResponse_nextToken = Lens.lens (\ListSecurityProfilesForTargetResponse' {nextToken} -> nextToken) (\s@ListSecurityProfilesForTargetResponse' {} a -> s {nextToken = a} :: ListSecurityProfilesForTargetResponse)

-- | A list of security profiles and their associated targets.
listSecurityProfilesForTargetResponse_securityProfileTargetMappings :: Lens.Lens' ListSecurityProfilesForTargetResponse (Prelude.Maybe [SecurityProfileTargetMapping])
listSecurityProfilesForTargetResponse_securityProfileTargetMappings = Lens.lens (\ListSecurityProfilesForTargetResponse' {securityProfileTargetMappings} -> securityProfileTargetMappings) (\s@ListSecurityProfilesForTargetResponse' {} a -> s {securityProfileTargetMappings = a} :: ListSecurityProfilesForTargetResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listSecurityProfilesForTargetResponse_httpStatus :: Lens.Lens' ListSecurityProfilesForTargetResponse Prelude.Int
listSecurityProfilesForTargetResponse_httpStatus = Lens.lens (\ListSecurityProfilesForTargetResponse' {httpStatus} -> httpStatus) (\s@ListSecurityProfilesForTargetResponse' {} a -> s {httpStatus = a} :: ListSecurityProfilesForTargetResponse)

instance
  Prelude.NFData
    ListSecurityProfilesForTargetResponse
  where
  rnf ListSecurityProfilesForTargetResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf securityProfileTargetMappings
      `Prelude.seq` Prelude.rnf httpStatus
