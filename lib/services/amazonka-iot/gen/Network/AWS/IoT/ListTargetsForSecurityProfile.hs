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
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions ListTargetsForSecurityProfile>
-- action.
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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListTargetsForSecurityProfile' smart constructor.
data ListTargetsForSecurityProfile = ListTargetsForSecurityProfile'
  { -- | The token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return at one time.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The security profile.
    securityProfileName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  ListTargetsForSecurityProfile
newListTargetsForSecurityProfile
  pSecurityProfileName_ =
    ListTargetsForSecurityProfile'
      { nextToken =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        securityProfileName = pSecurityProfileName_
      }

-- | The token for the next set of results.
listTargetsForSecurityProfile_nextToken :: Lens.Lens' ListTargetsForSecurityProfile (Prelude.Maybe Prelude.Text)
listTargetsForSecurityProfile_nextToken = Lens.lens (\ListTargetsForSecurityProfile' {nextToken} -> nextToken) (\s@ListTargetsForSecurityProfile' {} a -> s {nextToken = a} :: ListTargetsForSecurityProfile)

-- | The maximum number of results to return at one time.
listTargetsForSecurityProfile_maxResults :: Lens.Lens' ListTargetsForSecurityProfile (Prelude.Maybe Prelude.Natural)
listTargetsForSecurityProfile_maxResults = Lens.lens (\ListTargetsForSecurityProfile' {maxResults} -> maxResults) (\s@ListTargetsForSecurityProfile' {} a -> s {maxResults = a} :: ListTargetsForSecurityProfile)

-- | The security profile.
listTargetsForSecurityProfile_securityProfileName :: Lens.Lens' ListTargetsForSecurityProfile Prelude.Text
listTargetsForSecurityProfile_securityProfileName = Lens.lens (\ListTargetsForSecurityProfile' {securityProfileName} -> securityProfileName) (\s@ListTargetsForSecurityProfile' {} a -> s {securityProfileName = a} :: ListTargetsForSecurityProfile)

instance Core.AWSPager ListTargetsForSecurityProfile where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTargetsForSecurityProfileResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listTargetsForSecurityProfileResponse_securityProfileTargets
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listTargetsForSecurityProfile_nextToken
          Lens..~ rs
          Lens.^? listTargetsForSecurityProfileResponse_nextToken
            Prelude.. Lens._Just

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
            Prelude.<$> ( x Core..?> "securityProfileTargets"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListTargetsForSecurityProfile

instance Prelude.NFData ListTargetsForSecurityProfile

instance Core.ToHeaders ListTargetsForSecurityProfile where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListTargetsForSecurityProfile where
  toPath ListTargetsForSecurityProfile' {..} =
    Prelude.mconcat
      [ "/security-profiles/",
        Core.toBS securityProfileName,
        "/targets"
      ]

instance Core.ToQuery ListTargetsForSecurityProfile where
  toQuery ListTargetsForSecurityProfile' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListTargetsForSecurityProfileResponse' smart constructor.
data ListTargetsForSecurityProfileResponse = ListTargetsForSecurityProfileResponse'
  { -- | The thing groups to which the security profile is attached.
    securityProfileTargets :: Prelude.Maybe [SecurityProfileTarget],
    -- | A token that can be used to retrieve the next set of results, or @null@
    -- if there are no additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListTargetsForSecurityProfileResponse
newListTargetsForSecurityProfileResponse pHttpStatus_ =
  ListTargetsForSecurityProfileResponse'
    { securityProfileTargets =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The thing groups to which the security profile is attached.
listTargetsForSecurityProfileResponse_securityProfileTargets :: Lens.Lens' ListTargetsForSecurityProfileResponse (Prelude.Maybe [SecurityProfileTarget])
listTargetsForSecurityProfileResponse_securityProfileTargets = Lens.lens (\ListTargetsForSecurityProfileResponse' {securityProfileTargets} -> securityProfileTargets) (\s@ListTargetsForSecurityProfileResponse' {} a -> s {securityProfileTargets = a} :: ListTargetsForSecurityProfileResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token that can be used to retrieve the next set of results, or @null@
-- if there are no additional results.
listTargetsForSecurityProfileResponse_nextToken :: Lens.Lens' ListTargetsForSecurityProfileResponse (Prelude.Maybe Prelude.Text)
listTargetsForSecurityProfileResponse_nextToken = Lens.lens (\ListTargetsForSecurityProfileResponse' {nextToken} -> nextToken) (\s@ListTargetsForSecurityProfileResponse' {} a -> s {nextToken = a} :: ListTargetsForSecurityProfileResponse)

-- | The response's http status code.
listTargetsForSecurityProfileResponse_httpStatus :: Lens.Lens' ListTargetsForSecurityProfileResponse Prelude.Int
listTargetsForSecurityProfileResponse_httpStatus = Lens.lens (\ListTargetsForSecurityProfileResponse' {httpStatus} -> httpStatus) (\s@ListTargetsForSecurityProfileResponse' {} a -> s {httpStatus = a} :: ListTargetsForSecurityProfileResponse)

instance
  Prelude.NFData
    ListTargetsForSecurityProfileResponse
