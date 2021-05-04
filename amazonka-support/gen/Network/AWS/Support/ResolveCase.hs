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
-- Module      : Network.AWS.Support.ResolveCase
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resolves a support case. This operation takes a @caseId@ and returns the
-- initial and final state of the case.
--
-- -   You must have a Business or Enterprise support plan to use the AWS
--     Support API.
--
-- -   If you call the AWS Support API from an account that does not have a
--     Business or Enterprise support plan, the
--     @SubscriptionRequiredException@ error message appears. For
--     information about changing your support plan, see
--     <http://aws.amazon.com/premiumsupport/ AWS Support>.
module Network.AWS.Support.ResolveCase
  ( -- * Creating a Request
    ResolveCase (..),
    newResolveCase,

    -- * Request Lenses
    resolveCase_caseId,

    -- * Destructuring the Response
    ResolveCaseResponse (..),
    newResolveCaseResponse,

    -- * Response Lenses
    resolveCaseResponse_finalCaseStatus,
    resolveCaseResponse_initialCaseStatus,
    resolveCaseResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Support.Types

-- | /See:/ 'newResolveCase' smart constructor.
data ResolveCase = ResolveCase'
  { -- | The AWS Support case ID requested or returned in the call. The case ID
    -- is an alphanumeric string formatted as shown in this example:
    -- case-/12345678910-2013-c4c1d2bf33c5cf47/
    caseId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ResolveCase' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'caseId', 'resolveCase_caseId' - The AWS Support case ID requested or returned in the call. The case ID
-- is an alphanumeric string formatted as shown in this example:
-- case-/12345678910-2013-c4c1d2bf33c5cf47/
newResolveCase ::
  ResolveCase
newResolveCase =
  ResolveCase' {caseId = Prelude.Nothing}

-- | The AWS Support case ID requested or returned in the call. The case ID
-- is an alphanumeric string formatted as shown in this example:
-- case-/12345678910-2013-c4c1d2bf33c5cf47/
resolveCase_caseId :: Lens.Lens' ResolveCase (Prelude.Maybe Prelude.Text)
resolveCase_caseId = Lens.lens (\ResolveCase' {caseId} -> caseId) (\s@ResolveCase' {} a -> s {caseId = a} :: ResolveCase)

instance Prelude.AWSRequest ResolveCase where
  type Rs ResolveCase = ResolveCaseResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ResolveCaseResponse'
            Prelude.<$> (x Prelude..?> "finalCaseStatus")
            Prelude.<*> (x Prelude..?> "initialCaseStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ResolveCase

instance Prelude.NFData ResolveCase

instance Prelude.ToHeaders ResolveCase where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSSupport_20130415.ResolveCase" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ResolveCase where
  toJSON ResolveCase' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("caseId" Prelude..=) Prelude.<$> caseId]
      )

instance Prelude.ToPath ResolveCase where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ResolveCase where
  toQuery = Prelude.const Prelude.mempty

-- | The status of the case returned by the ResolveCase operation.
--
-- /See:/ 'newResolveCaseResponse' smart constructor.
data ResolveCaseResponse = ResolveCaseResponse'
  { -- | The status of the case after the ResolveCase request was processed.
    finalCaseStatus :: Prelude.Maybe Prelude.Text,
    -- | The status of the case when the ResolveCase request was sent.
    initialCaseStatus :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ResolveCaseResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'finalCaseStatus', 'resolveCaseResponse_finalCaseStatus' - The status of the case after the ResolveCase request was processed.
--
-- 'initialCaseStatus', 'resolveCaseResponse_initialCaseStatus' - The status of the case when the ResolveCase request was sent.
--
-- 'httpStatus', 'resolveCaseResponse_httpStatus' - The response's http status code.
newResolveCaseResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ResolveCaseResponse
newResolveCaseResponse pHttpStatus_ =
  ResolveCaseResponse'
    { finalCaseStatus =
        Prelude.Nothing,
      initialCaseStatus = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the case after the ResolveCase request was processed.
resolveCaseResponse_finalCaseStatus :: Lens.Lens' ResolveCaseResponse (Prelude.Maybe Prelude.Text)
resolveCaseResponse_finalCaseStatus = Lens.lens (\ResolveCaseResponse' {finalCaseStatus} -> finalCaseStatus) (\s@ResolveCaseResponse' {} a -> s {finalCaseStatus = a} :: ResolveCaseResponse)

-- | The status of the case when the ResolveCase request was sent.
resolveCaseResponse_initialCaseStatus :: Lens.Lens' ResolveCaseResponse (Prelude.Maybe Prelude.Text)
resolveCaseResponse_initialCaseStatus = Lens.lens (\ResolveCaseResponse' {initialCaseStatus} -> initialCaseStatus) (\s@ResolveCaseResponse' {} a -> s {initialCaseStatus = a} :: ResolveCaseResponse)

-- | The response's http status code.
resolveCaseResponse_httpStatus :: Lens.Lens' ResolveCaseResponse Prelude.Int
resolveCaseResponse_httpStatus = Lens.lens (\ResolveCaseResponse' {httpStatus} -> httpStatus) (\s@ResolveCaseResponse' {} a -> s {httpStatus = a} :: ResolveCaseResponse)

instance Prelude.NFData ResolveCaseResponse
