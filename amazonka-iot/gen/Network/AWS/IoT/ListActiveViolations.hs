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
-- Module      : Network.AWS.IoT.ListActiveViolations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the active violations for a given Device Defender security
-- profile.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListActiveViolations
  ( -- * Creating a Request
    ListActiveViolations (..),
    newListActiveViolations,

    -- * Request Lenses
    listActiveViolations_nextToken,
    listActiveViolations_maxResults,
    listActiveViolations_thingName,
    listActiveViolations_securityProfileName,
    listActiveViolations_listSuppressedAlerts,
    listActiveViolations_behaviorCriteriaType,

    -- * Destructuring the Response
    ListActiveViolationsResponse (..),
    newListActiveViolationsResponse,

    -- * Response Lenses
    listActiveViolationsResponse_nextToken,
    listActiveViolationsResponse_activeViolations,
    listActiveViolationsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListActiveViolations' smart constructor.
data ListActiveViolations = ListActiveViolations'
  { -- | The token for the next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return at one time.
    maxResults :: Core.Maybe Core.Natural,
    -- | The name of the thing whose active violations are listed.
    thingName :: Core.Maybe Core.Text,
    -- | The name of the Device Defender security profile for which violations
    -- are listed.
    securityProfileName :: Core.Maybe Core.Text,
    -- | A list of all suppressed alerts.
    listSuppressedAlerts :: Core.Maybe Core.Bool,
    -- | The criteria for a behavior.
    behaviorCriteriaType :: Core.Maybe BehaviorCriteriaType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListActiveViolations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listActiveViolations_nextToken' - The token for the next set of results.
--
-- 'maxResults', 'listActiveViolations_maxResults' - The maximum number of results to return at one time.
--
-- 'thingName', 'listActiveViolations_thingName' - The name of the thing whose active violations are listed.
--
-- 'securityProfileName', 'listActiveViolations_securityProfileName' - The name of the Device Defender security profile for which violations
-- are listed.
--
-- 'listSuppressedAlerts', 'listActiveViolations_listSuppressedAlerts' - A list of all suppressed alerts.
--
-- 'behaviorCriteriaType', 'listActiveViolations_behaviorCriteriaType' - The criteria for a behavior.
newListActiveViolations ::
  ListActiveViolations
newListActiveViolations =
  ListActiveViolations'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      thingName = Core.Nothing,
      securityProfileName = Core.Nothing,
      listSuppressedAlerts = Core.Nothing,
      behaviorCriteriaType = Core.Nothing
    }

-- | The token for the next set of results.
listActiveViolations_nextToken :: Lens.Lens' ListActiveViolations (Core.Maybe Core.Text)
listActiveViolations_nextToken = Lens.lens (\ListActiveViolations' {nextToken} -> nextToken) (\s@ListActiveViolations' {} a -> s {nextToken = a} :: ListActiveViolations)

-- | The maximum number of results to return at one time.
listActiveViolations_maxResults :: Lens.Lens' ListActiveViolations (Core.Maybe Core.Natural)
listActiveViolations_maxResults = Lens.lens (\ListActiveViolations' {maxResults} -> maxResults) (\s@ListActiveViolations' {} a -> s {maxResults = a} :: ListActiveViolations)

-- | The name of the thing whose active violations are listed.
listActiveViolations_thingName :: Lens.Lens' ListActiveViolations (Core.Maybe Core.Text)
listActiveViolations_thingName = Lens.lens (\ListActiveViolations' {thingName} -> thingName) (\s@ListActiveViolations' {} a -> s {thingName = a} :: ListActiveViolations)

-- | The name of the Device Defender security profile for which violations
-- are listed.
listActiveViolations_securityProfileName :: Lens.Lens' ListActiveViolations (Core.Maybe Core.Text)
listActiveViolations_securityProfileName = Lens.lens (\ListActiveViolations' {securityProfileName} -> securityProfileName) (\s@ListActiveViolations' {} a -> s {securityProfileName = a} :: ListActiveViolations)

-- | A list of all suppressed alerts.
listActiveViolations_listSuppressedAlerts :: Lens.Lens' ListActiveViolations (Core.Maybe Core.Bool)
listActiveViolations_listSuppressedAlerts = Lens.lens (\ListActiveViolations' {listSuppressedAlerts} -> listSuppressedAlerts) (\s@ListActiveViolations' {} a -> s {listSuppressedAlerts = a} :: ListActiveViolations)

-- | The criteria for a behavior.
listActiveViolations_behaviorCriteriaType :: Lens.Lens' ListActiveViolations (Core.Maybe BehaviorCriteriaType)
listActiveViolations_behaviorCriteriaType = Lens.lens (\ListActiveViolations' {behaviorCriteriaType} -> behaviorCriteriaType) (\s@ListActiveViolations' {} a -> s {behaviorCriteriaType = a} :: ListActiveViolations)

instance Core.AWSPager ListActiveViolations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listActiveViolationsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listActiveViolationsResponse_activeViolations
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listActiveViolations_nextToken
          Lens..~ rs
          Lens.^? listActiveViolationsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListActiveViolations where
  type
    AWSResponse ListActiveViolations =
      ListActiveViolationsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListActiveViolationsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "activeViolations" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListActiveViolations

instance Core.NFData ListActiveViolations

instance Core.ToHeaders ListActiveViolations where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListActiveViolations where
  toPath = Core.const "/active-violations"

instance Core.ToQuery ListActiveViolations where
  toQuery ListActiveViolations' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults,
        "thingName" Core.=: thingName,
        "securityProfileName" Core.=: securityProfileName,
        "listSuppressedAlerts" Core.=: listSuppressedAlerts,
        "behaviorCriteriaType" Core.=: behaviorCriteriaType
      ]

-- | /See:/ 'newListActiveViolationsResponse' smart constructor.
data ListActiveViolationsResponse = ListActiveViolationsResponse'
  { -- | A token that can be used to retrieve the next set of results, or @null@
    -- if there are no additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | The list of active violations.
    activeViolations :: Core.Maybe [ActiveViolation],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListActiveViolationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listActiveViolationsResponse_nextToken' - A token that can be used to retrieve the next set of results, or @null@
-- if there are no additional results.
--
-- 'activeViolations', 'listActiveViolationsResponse_activeViolations' - The list of active violations.
--
-- 'httpStatus', 'listActiveViolationsResponse_httpStatus' - The response's http status code.
newListActiveViolationsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListActiveViolationsResponse
newListActiveViolationsResponse pHttpStatus_ =
  ListActiveViolationsResponse'
    { nextToken =
        Core.Nothing,
      activeViolations = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token that can be used to retrieve the next set of results, or @null@
-- if there are no additional results.
listActiveViolationsResponse_nextToken :: Lens.Lens' ListActiveViolationsResponse (Core.Maybe Core.Text)
listActiveViolationsResponse_nextToken = Lens.lens (\ListActiveViolationsResponse' {nextToken} -> nextToken) (\s@ListActiveViolationsResponse' {} a -> s {nextToken = a} :: ListActiveViolationsResponse)

-- | The list of active violations.
listActiveViolationsResponse_activeViolations :: Lens.Lens' ListActiveViolationsResponse (Core.Maybe [ActiveViolation])
listActiveViolationsResponse_activeViolations = Lens.lens (\ListActiveViolationsResponse' {activeViolations} -> activeViolations) (\s@ListActiveViolationsResponse' {} a -> s {activeViolations = a} :: ListActiveViolationsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listActiveViolationsResponse_httpStatus :: Lens.Lens' ListActiveViolationsResponse Core.Int
listActiveViolationsResponse_httpStatus = Lens.lens (\ListActiveViolationsResponse' {httpStatus} -> httpStatus) (\s@ListActiveViolationsResponse' {} a -> s {httpStatus = a} :: ListActiveViolationsResponse)

instance Core.NFData ListActiveViolationsResponse
