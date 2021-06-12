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
-- Module      : Network.AWS.IoT.ListSecurityProfilesForTarget
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the Device Defender security profiles attached to a target (thing
-- group).
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListSecurityProfilesForTarget
  ( -- * Creating a Request
    ListSecurityProfilesForTarget (..),
    newListSecurityProfilesForTarget,

    -- * Request Lenses
    listSecurityProfilesForTarget_nextToken,
    listSecurityProfilesForTarget_maxResults,
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

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListSecurityProfilesForTarget' smart constructor.
data ListSecurityProfilesForTarget = ListSecurityProfilesForTarget'
  { -- | The token for the next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return at one time.
    maxResults :: Core.Maybe Core.Natural,
    -- | If true, return child groups too.
    recursive :: Core.Maybe Core.Bool,
    -- | The ARN of the target (thing group) whose attached security profiles you
    -- want to get.
    securityProfileTargetArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListSecurityProfilesForTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSecurityProfilesForTarget_nextToken' - The token for the next set of results.
--
-- 'maxResults', 'listSecurityProfilesForTarget_maxResults' - The maximum number of results to return at one time.
--
-- 'recursive', 'listSecurityProfilesForTarget_recursive' - If true, return child groups too.
--
-- 'securityProfileTargetArn', 'listSecurityProfilesForTarget_securityProfileTargetArn' - The ARN of the target (thing group) whose attached security profiles you
-- want to get.
newListSecurityProfilesForTarget ::
  -- | 'securityProfileTargetArn'
  Core.Text ->
  ListSecurityProfilesForTarget
newListSecurityProfilesForTarget
  pSecurityProfileTargetArn_ =
    ListSecurityProfilesForTarget'
      { nextToken =
          Core.Nothing,
        maxResults = Core.Nothing,
        recursive = Core.Nothing,
        securityProfileTargetArn =
          pSecurityProfileTargetArn_
      }

-- | The token for the next set of results.
listSecurityProfilesForTarget_nextToken :: Lens.Lens' ListSecurityProfilesForTarget (Core.Maybe Core.Text)
listSecurityProfilesForTarget_nextToken = Lens.lens (\ListSecurityProfilesForTarget' {nextToken} -> nextToken) (\s@ListSecurityProfilesForTarget' {} a -> s {nextToken = a} :: ListSecurityProfilesForTarget)

-- | The maximum number of results to return at one time.
listSecurityProfilesForTarget_maxResults :: Lens.Lens' ListSecurityProfilesForTarget (Core.Maybe Core.Natural)
listSecurityProfilesForTarget_maxResults = Lens.lens (\ListSecurityProfilesForTarget' {maxResults} -> maxResults) (\s@ListSecurityProfilesForTarget' {} a -> s {maxResults = a} :: ListSecurityProfilesForTarget)

-- | If true, return child groups too.
listSecurityProfilesForTarget_recursive :: Lens.Lens' ListSecurityProfilesForTarget (Core.Maybe Core.Bool)
listSecurityProfilesForTarget_recursive = Lens.lens (\ListSecurityProfilesForTarget' {recursive} -> recursive) (\s@ListSecurityProfilesForTarget' {} a -> s {recursive = a} :: ListSecurityProfilesForTarget)

-- | The ARN of the target (thing group) whose attached security profiles you
-- want to get.
listSecurityProfilesForTarget_securityProfileTargetArn :: Lens.Lens' ListSecurityProfilesForTarget Core.Text
listSecurityProfilesForTarget_securityProfileTargetArn = Lens.lens (\ListSecurityProfilesForTarget' {securityProfileTargetArn} -> securityProfileTargetArn) (\s@ListSecurityProfilesForTarget' {} a -> s {securityProfileTargetArn = a} :: ListSecurityProfilesForTarget)

instance Core.AWSPager ListSecurityProfilesForTarget where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSecurityProfilesForTargetResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listSecurityProfilesForTargetResponse_securityProfileTargetMappings
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listSecurityProfilesForTarget_nextToken
          Lens..~ rs
          Lens.^? listSecurityProfilesForTargetResponse_nextToken
            Core.. Lens._Just

instance
  Core.AWSRequest
    ListSecurityProfilesForTarget
  where
  type
    AWSResponse ListSecurityProfilesForTarget =
      ListSecurityProfilesForTargetResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSecurityProfilesForTargetResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> ( x Core..?> "securityProfileTargetMappings"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListSecurityProfilesForTarget

instance Core.NFData ListSecurityProfilesForTarget

instance Core.ToHeaders ListSecurityProfilesForTarget where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListSecurityProfilesForTarget where
  toPath = Core.const "/security-profiles-for-target"

instance Core.ToQuery ListSecurityProfilesForTarget where
  toQuery ListSecurityProfilesForTarget' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults,
        "recursive" Core.=: recursive,
        "securityProfileTargetArn"
          Core.=: securityProfileTargetArn
      ]

-- | /See:/ 'newListSecurityProfilesForTargetResponse' smart constructor.
data ListSecurityProfilesForTargetResponse = ListSecurityProfilesForTargetResponse'
  { -- | A token that can be used to retrieve the next set of results, or @null@
    -- if there are no additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of security profiles and their associated targets.
    securityProfileTargetMappings :: Core.Maybe [SecurityProfileTargetMapping],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ListSecurityProfilesForTargetResponse
newListSecurityProfilesForTargetResponse pHttpStatus_ =
  ListSecurityProfilesForTargetResponse'
    { nextToken =
        Core.Nothing,
      securityProfileTargetMappings =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token that can be used to retrieve the next set of results, or @null@
-- if there are no additional results.
listSecurityProfilesForTargetResponse_nextToken :: Lens.Lens' ListSecurityProfilesForTargetResponse (Core.Maybe Core.Text)
listSecurityProfilesForTargetResponse_nextToken = Lens.lens (\ListSecurityProfilesForTargetResponse' {nextToken} -> nextToken) (\s@ListSecurityProfilesForTargetResponse' {} a -> s {nextToken = a} :: ListSecurityProfilesForTargetResponse)

-- | A list of security profiles and their associated targets.
listSecurityProfilesForTargetResponse_securityProfileTargetMappings :: Lens.Lens' ListSecurityProfilesForTargetResponse (Core.Maybe [SecurityProfileTargetMapping])
listSecurityProfilesForTargetResponse_securityProfileTargetMappings = Lens.lens (\ListSecurityProfilesForTargetResponse' {securityProfileTargetMappings} -> securityProfileTargetMappings) (\s@ListSecurityProfilesForTargetResponse' {} a -> s {securityProfileTargetMappings = a} :: ListSecurityProfilesForTargetResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listSecurityProfilesForTargetResponse_httpStatus :: Lens.Lens' ListSecurityProfilesForTargetResponse Core.Int
listSecurityProfilesForTargetResponse_httpStatus = Lens.lens (\ListSecurityProfilesForTargetResponse' {httpStatus} -> httpStatus) (\s@ListSecurityProfilesForTargetResponse' {} a -> s {httpStatus = a} :: ListSecurityProfilesForTargetResponse)

instance
  Core.NFData
    ListSecurityProfilesForTargetResponse
