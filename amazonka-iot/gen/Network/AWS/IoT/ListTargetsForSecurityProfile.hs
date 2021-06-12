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
-- Module      : Network.AWS.IoT.ListTargetsForSecurityProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the targets (thing groups) associated with a given Device Defender
-- security profile.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListTargetsForSecurityProfile
  ( -- * Creating a Request
    ListTargetsForSecurityProfile (..),
    newListTargetsForSecurityProfile,

    -- * Request Lenses
    listTargetsForSecurityProfile_nextToken,
    listTargetsForSecurityProfile_maxResults,
    listTargetsForSecurityProfile_securityProfileName,

    -- * Destructuring the Response
    ListTargetsForSecurityProfileResponse (..),
    newListTargetsForSecurityProfileResponse,

    -- * Response Lenses
    listTargetsForSecurityProfileResponse_securityProfileTargets,
    listTargetsForSecurityProfileResponse_nextToken,
    listTargetsForSecurityProfileResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListTargetsForSecurityProfile' smart constructor.
data ListTargetsForSecurityProfile = ListTargetsForSecurityProfile'
  { -- | The token for the next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return at one time.
    maxResults :: Core.Maybe Core.Natural,
    -- | The security profile.
    securityProfileName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListTargetsForSecurityProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTargetsForSecurityProfile_nextToken' - The token for the next set of results.
--
-- 'maxResults', 'listTargetsForSecurityProfile_maxResults' - The maximum number of results to return at one time.
--
-- 'securityProfileName', 'listTargetsForSecurityProfile_securityProfileName' - The security profile.
newListTargetsForSecurityProfile ::
  -- | 'securityProfileName'
  Core.Text ->
  ListTargetsForSecurityProfile
newListTargetsForSecurityProfile
  pSecurityProfileName_ =
    ListTargetsForSecurityProfile'
      { nextToken =
          Core.Nothing,
        maxResults = Core.Nothing,
        securityProfileName = pSecurityProfileName_
      }

-- | The token for the next set of results.
listTargetsForSecurityProfile_nextToken :: Lens.Lens' ListTargetsForSecurityProfile (Core.Maybe Core.Text)
listTargetsForSecurityProfile_nextToken = Lens.lens (\ListTargetsForSecurityProfile' {nextToken} -> nextToken) (\s@ListTargetsForSecurityProfile' {} a -> s {nextToken = a} :: ListTargetsForSecurityProfile)

-- | The maximum number of results to return at one time.
listTargetsForSecurityProfile_maxResults :: Lens.Lens' ListTargetsForSecurityProfile (Core.Maybe Core.Natural)
listTargetsForSecurityProfile_maxResults = Lens.lens (\ListTargetsForSecurityProfile' {maxResults} -> maxResults) (\s@ListTargetsForSecurityProfile' {} a -> s {maxResults = a} :: ListTargetsForSecurityProfile)

-- | The security profile.
listTargetsForSecurityProfile_securityProfileName :: Lens.Lens' ListTargetsForSecurityProfile Core.Text
listTargetsForSecurityProfile_securityProfileName = Lens.lens (\ListTargetsForSecurityProfile' {securityProfileName} -> securityProfileName) (\s@ListTargetsForSecurityProfile' {} a -> s {securityProfileName = a} :: ListTargetsForSecurityProfile)

instance Core.AWSPager ListTargetsForSecurityProfile where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTargetsForSecurityProfileResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listTargetsForSecurityProfileResponse_securityProfileTargets
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listTargetsForSecurityProfile_nextToken
          Lens..~ rs
          Lens.^? listTargetsForSecurityProfileResponse_nextToken
            Core.. Lens._Just

instance
  Core.AWSRequest
    ListTargetsForSecurityProfile
  where
  type
    AWSResponse ListTargetsForSecurityProfile =
      ListTargetsForSecurityProfileResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTargetsForSecurityProfileResponse'
            Core.<$> ( x Core..?> "securityProfileTargets"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListTargetsForSecurityProfile

instance Core.NFData ListTargetsForSecurityProfile

instance Core.ToHeaders ListTargetsForSecurityProfile where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListTargetsForSecurityProfile where
  toPath ListTargetsForSecurityProfile' {..} =
    Core.mconcat
      [ "/security-profiles/",
        Core.toBS securityProfileName,
        "/targets"
      ]

instance Core.ToQuery ListTargetsForSecurityProfile where
  toQuery ListTargetsForSecurityProfile' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListTargetsForSecurityProfileResponse' smart constructor.
data ListTargetsForSecurityProfileResponse = ListTargetsForSecurityProfileResponse'
  { -- | The thing groups to which the security profile is attached.
    securityProfileTargets :: Core.Maybe [SecurityProfileTarget],
    -- | A token that can be used to retrieve the next set of results, or @null@
    -- if there are no additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListTargetsForSecurityProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityProfileTargets', 'listTargetsForSecurityProfileResponse_securityProfileTargets' - The thing groups to which the security profile is attached.
--
-- 'nextToken', 'listTargetsForSecurityProfileResponse_nextToken' - A token that can be used to retrieve the next set of results, or @null@
-- if there are no additional results.
--
-- 'httpStatus', 'listTargetsForSecurityProfileResponse_httpStatus' - The response's http status code.
newListTargetsForSecurityProfileResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListTargetsForSecurityProfileResponse
newListTargetsForSecurityProfileResponse pHttpStatus_ =
  ListTargetsForSecurityProfileResponse'
    { securityProfileTargets =
        Core.Nothing,
      nextToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The thing groups to which the security profile is attached.
listTargetsForSecurityProfileResponse_securityProfileTargets :: Lens.Lens' ListTargetsForSecurityProfileResponse (Core.Maybe [SecurityProfileTarget])
listTargetsForSecurityProfileResponse_securityProfileTargets = Lens.lens (\ListTargetsForSecurityProfileResponse' {securityProfileTargets} -> securityProfileTargets) (\s@ListTargetsForSecurityProfileResponse' {} a -> s {securityProfileTargets = a} :: ListTargetsForSecurityProfileResponse) Core.. Lens.mapping Lens._Coerce

-- | A token that can be used to retrieve the next set of results, or @null@
-- if there are no additional results.
listTargetsForSecurityProfileResponse_nextToken :: Lens.Lens' ListTargetsForSecurityProfileResponse (Core.Maybe Core.Text)
listTargetsForSecurityProfileResponse_nextToken = Lens.lens (\ListTargetsForSecurityProfileResponse' {nextToken} -> nextToken) (\s@ListTargetsForSecurityProfileResponse' {} a -> s {nextToken = a} :: ListTargetsForSecurityProfileResponse)

-- | The response's http status code.
listTargetsForSecurityProfileResponse_httpStatus :: Lens.Lens' ListTargetsForSecurityProfileResponse Core.Int
listTargetsForSecurityProfileResponse_httpStatus = Lens.lens (\ListTargetsForSecurityProfileResponse' {httpStatus} -> httpStatus) (\s@ListTargetsForSecurityProfileResponse' {} a -> s {httpStatus = a} :: ListTargetsForSecurityProfileResponse)

instance
  Core.NFData
    ListTargetsForSecurityProfileResponse
