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
-- Module      : Amazonka.IoT.ListActiveViolations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the active violations for a given Device Defender security
-- profile.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions ListActiveViolations>
-- action.
--
-- This operation returns paginated results.
module Amazonka.IoT.ListActiveViolations
  ( -- * Creating a Request
    ListActiveViolations (..),
    newListActiveViolations,

    -- * Request Lenses
    listActiveViolations_behaviorCriteriaType,
    listActiveViolations_listSuppressedAlerts,
    listActiveViolations_maxResults,
    listActiveViolations_nextToken,
    listActiveViolations_securityProfileName,
    listActiveViolations_thingName,
    listActiveViolations_verificationState,

    -- * Destructuring the Response
    ListActiveViolationsResponse (..),
    newListActiveViolationsResponse,

    -- * Response Lenses
    listActiveViolationsResponse_activeViolations,
    listActiveViolationsResponse_nextToken,
    listActiveViolationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListActiveViolations' smart constructor.
data ListActiveViolations = ListActiveViolations'
  { -- | The criteria for a behavior.
    behaviorCriteriaType :: Prelude.Maybe BehaviorCriteriaType,
    -- | A list of all suppressed alerts.
    listSuppressedAlerts :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of results to return at one time.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the Device Defender security profile for which violations
    -- are listed.
    securityProfileName :: Prelude.Maybe Prelude.Text,
    -- | The name of the thing whose active violations are listed.
    thingName :: Prelude.Maybe Prelude.Text,
    -- | The verification state of the violation (detect alarm).
    verificationState :: Prelude.Maybe VerificationState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListActiveViolations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'behaviorCriteriaType', 'listActiveViolations_behaviorCriteriaType' - The criteria for a behavior.
--
-- 'listSuppressedAlerts', 'listActiveViolations_listSuppressedAlerts' - A list of all suppressed alerts.
--
-- 'maxResults', 'listActiveViolations_maxResults' - The maximum number of results to return at one time.
--
-- 'nextToken', 'listActiveViolations_nextToken' - The token for the next set of results.
--
-- 'securityProfileName', 'listActiveViolations_securityProfileName' - The name of the Device Defender security profile for which violations
-- are listed.
--
-- 'thingName', 'listActiveViolations_thingName' - The name of the thing whose active violations are listed.
--
-- 'verificationState', 'listActiveViolations_verificationState' - The verification state of the violation (detect alarm).
newListActiveViolations ::
  ListActiveViolations
newListActiveViolations =
  ListActiveViolations'
    { behaviorCriteriaType =
        Prelude.Nothing,
      listSuppressedAlerts = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      securityProfileName = Prelude.Nothing,
      thingName = Prelude.Nothing,
      verificationState = Prelude.Nothing
    }

-- | The criteria for a behavior.
listActiveViolations_behaviorCriteriaType :: Lens.Lens' ListActiveViolations (Prelude.Maybe BehaviorCriteriaType)
listActiveViolations_behaviorCriteriaType = Lens.lens (\ListActiveViolations' {behaviorCriteriaType} -> behaviorCriteriaType) (\s@ListActiveViolations' {} a -> s {behaviorCriteriaType = a} :: ListActiveViolations)

-- | A list of all suppressed alerts.
listActiveViolations_listSuppressedAlerts :: Lens.Lens' ListActiveViolations (Prelude.Maybe Prelude.Bool)
listActiveViolations_listSuppressedAlerts = Lens.lens (\ListActiveViolations' {listSuppressedAlerts} -> listSuppressedAlerts) (\s@ListActiveViolations' {} a -> s {listSuppressedAlerts = a} :: ListActiveViolations)

-- | The maximum number of results to return at one time.
listActiveViolations_maxResults :: Lens.Lens' ListActiveViolations (Prelude.Maybe Prelude.Natural)
listActiveViolations_maxResults = Lens.lens (\ListActiveViolations' {maxResults} -> maxResults) (\s@ListActiveViolations' {} a -> s {maxResults = a} :: ListActiveViolations)

-- | The token for the next set of results.
listActiveViolations_nextToken :: Lens.Lens' ListActiveViolations (Prelude.Maybe Prelude.Text)
listActiveViolations_nextToken = Lens.lens (\ListActiveViolations' {nextToken} -> nextToken) (\s@ListActiveViolations' {} a -> s {nextToken = a} :: ListActiveViolations)

-- | The name of the Device Defender security profile for which violations
-- are listed.
listActiveViolations_securityProfileName :: Lens.Lens' ListActiveViolations (Prelude.Maybe Prelude.Text)
listActiveViolations_securityProfileName = Lens.lens (\ListActiveViolations' {securityProfileName} -> securityProfileName) (\s@ListActiveViolations' {} a -> s {securityProfileName = a} :: ListActiveViolations)

-- | The name of the thing whose active violations are listed.
listActiveViolations_thingName :: Lens.Lens' ListActiveViolations (Prelude.Maybe Prelude.Text)
listActiveViolations_thingName = Lens.lens (\ListActiveViolations' {thingName} -> thingName) (\s@ListActiveViolations' {} a -> s {thingName = a} :: ListActiveViolations)

-- | The verification state of the violation (detect alarm).
listActiveViolations_verificationState :: Lens.Lens' ListActiveViolations (Prelude.Maybe VerificationState)
listActiveViolations_verificationState = Lens.lens (\ListActiveViolations' {verificationState} -> verificationState) (\s@ListActiveViolations' {} a -> s {verificationState = a} :: ListActiveViolations)

instance Core.AWSPager ListActiveViolations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listActiveViolationsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listActiveViolationsResponse_activeViolations
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listActiveViolations_nextToken
          Lens..~ rs
          Lens.^? listActiveViolationsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListActiveViolations where
  type
    AWSResponse ListActiveViolations =
      ListActiveViolationsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListActiveViolationsResponse'
            Prelude.<$> ( x Data..?> "activeViolations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListActiveViolations where
  hashWithSalt _salt ListActiveViolations' {..} =
    _salt `Prelude.hashWithSalt` behaviorCriteriaType
      `Prelude.hashWithSalt` listSuppressedAlerts
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` securityProfileName
      `Prelude.hashWithSalt` thingName
      `Prelude.hashWithSalt` verificationState

instance Prelude.NFData ListActiveViolations where
  rnf ListActiveViolations' {..} =
    Prelude.rnf behaviorCriteriaType
      `Prelude.seq` Prelude.rnf listSuppressedAlerts
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf securityProfileName
      `Prelude.seq` Prelude.rnf thingName
      `Prelude.seq` Prelude.rnf verificationState

instance Data.ToHeaders ListActiveViolations where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListActiveViolations where
  toPath = Prelude.const "/active-violations"

instance Data.ToQuery ListActiveViolations where
  toQuery ListActiveViolations' {..} =
    Prelude.mconcat
      [ "behaviorCriteriaType" Data.=: behaviorCriteriaType,
        "listSuppressedAlerts" Data.=: listSuppressedAlerts,
        "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "securityProfileName" Data.=: securityProfileName,
        "thingName" Data.=: thingName,
        "verificationState" Data.=: verificationState
      ]

-- | /See:/ 'newListActiveViolationsResponse' smart constructor.
data ListActiveViolationsResponse = ListActiveViolationsResponse'
  { -- | The list of active violations.
    activeViolations :: Prelude.Maybe [ActiveViolation],
    -- | A token that can be used to retrieve the next set of results, or @null@
    -- if there are no additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListActiveViolationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activeViolations', 'listActiveViolationsResponse_activeViolations' - The list of active violations.
--
-- 'nextToken', 'listActiveViolationsResponse_nextToken' - A token that can be used to retrieve the next set of results, or @null@
-- if there are no additional results.
--
-- 'httpStatus', 'listActiveViolationsResponse_httpStatus' - The response's http status code.
newListActiveViolationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListActiveViolationsResponse
newListActiveViolationsResponse pHttpStatus_ =
  ListActiveViolationsResponse'
    { activeViolations =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of active violations.
listActiveViolationsResponse_activeViolations :: Lens.Lens' ListActiveViolationsResponse (Prelude.Maybe [ActiveViolation])
listActiveViolationsResponse_activeViolations = Lens.lens (\ListActiveViolationsResponse' {activeViolations} -> activeViolations) (\s@ListActiveViolationsResponse' {} a -> s {activeViolations = a} :: ListActiveViolationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token that can be used to retrieve the next set of results, or @null@
-- if there are no additional results.
listActiveViolationsResponse_nextToken :: Lens.Lens' ListActiveViolationsResponse (Prelude.Maybe Prelude.Text)
listActiveViolationsResponse_nextToken = Lens.lens (\ListActiveViolationsResponse' {nextToken} -> nextToken) (\s@ListActiveViolationsResponse' {} a -> s {nextToken = a} :: ListActiveViolationsResponse)

-- | The response's http status code.
listActiveViolationsResponse_httpStatus :: Lens.Lens' ListActiveViolationsResponse Prelude.Int
listActiveViolationsResponse_httpStatus = Lens.lens (\ListActiveViolationsResponse' {httpStatus} -> httpStatus) (\s@ListActiveViolationsResponse' {} a -> s {httpStatus = a} :: ListActiveViolationsResponse)

instance Prelude.NFData ListActiveViolationsResponse where
  rnf ListActiveViolationsResponse' {..} =
    Prelude.rnf activeViolations
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
