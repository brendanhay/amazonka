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
-- Module      : Amazonka.AuditManager.StartAssessmentFrameworkShare
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a share request for a custom framework in Audit Manager.
--
-- The share request specifies a recipient and notifies them that a custom
-- framework is available. Recipients have 120 days to accept or decline
-- the request. If no action is taken, the share request expires.
--
-- When you create a share request, Audit Manager stores a snapshot of your
-- custom framework in the US East (N. Virginia) Amazon Web Services
-- Region. Audit Manager also stores a backup of the same snapshot in the
-- US West (Oregon) Amazon Web Services Region.
--
-- Audit Manager deletes the snapshot and the backup snapshot when one of
-- the following events occurs:
--
-- -   The sender revokes the share request.
--
-- -   The recipient declines the share request.
--
-- -   The recipient encounters an error and doesn\'t successfully accept
--     the share request.
--
-- -   The share request expires before the recipient responds to the
--     request.
--
-- When a sender
-- <https://docs.aws.amazon.com/audit-manager/latest/userguide/framework-sharing.html#framework-sharing-resend resends a share request>,
-- the snapshot is replaced with an updated version that corresponds with
-- the latest version of the custom framework.
--
-- When a recipient accepts a share request, the snapshot is replicated
-- into their Amazon Web Services account under the Amazon Web Services
-- Region that was specified in the share request.
--
-- When you invoke the @StartAssessmentFrameworkShare@ API, you are about
-- to share a custom framework with another Amazon Web Services account.
-- You may not share a custom framework that is derived from a standard
-- framework if the standard framework is designated as not eligible for
-- sharing by Amazon Web Services, unless you have obtained permission to
-- do so from the owner of the standard framework. To learn more about
-- which standard frameworks are eligible for sharing, see
-- <https://docs.aws.amazon.com/audit-manager/latest/userguide/share-custom-framework-concepts-and-terminology.html#eligibility Framework sharing eligibility>
-- in the /Audit Manager User Guide/.
module Amazonka.AuditManager.StartAssessmentFrameworkShare
  ( -- * Creating a Request
    StartAssessmentFrameworkShare (..),
    newStartAssessmentFrameworkShare,

    -- * Request Lenses
    startAssessmentFrameworkShare_comment,
    startAssessmentFrameworkShare_frameworkId,
    startAssessmentFrameworkShare_destinationAccount,
    startAssessmentFrameworkShare_destinationRegion,

    -- * Destructuring the Response
    StartAssessmentFrameworkShareResponse (..),
    newStartAssessmentFrameworkShareResponse,

    -- * Response Lenses
    startAssessmentFrameworkShareResponse_assessmentFrameworkShareRequest,
    startAssessmentFrameworkShareResponse_httpStatus,
  )
where

import Amazonka.AuditManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartAssessmentFrameworkShare' smart constructor.
data StartAssessmentFrameworkShare = StartAssessmentFrameworkShare'
  { -- | An optional comment from the sender about the share request.
    comment :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the custom framework to be shared.
    frameworkId :: Prelude.Text,
    -- | The Amazon Web Services account of the recipient.
    destinationAccount :: Prelude.Text,
    -- | The Amazon Web Services Region of the recipient.
    destinationRegion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartAssessmentFrameworkShare' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comment', 'startAssessmentFrameworkShare_comment' - An optional comment from the sender about the share request.
--
-- 'frameworkId', 'startAssessmentFrameworkShare_frameworkId' - The unique identifier for the custom framework to be shared.
--
-- 'destinationAccount', 'startAssessmentFrameworkShare_destinationAccount' - The Amazon Web Services account of the recipient.
--
-- 'destinationRegion', 'startAssessmentFrameworkShare_destinationRegion' - The Amazon Web Services Region of the recipient.
newStartAssessmentFrameworkShare ::
  -- | 'frameworkId'
  Prelude.Text ->
  -- | 'destinationAccount'
  Prelude.Text ->
  -- | 'destinationRegion'
  Prelude.Text ->
  StartAssessmentFrameworkShare
newStartAssessmentFrameworkShare
  pFrameworkId_
  pDestinationAccount_
  pDestinationRegion_ =
    StartAssessmentFrameworkShare'
      { comment =
          Prelude.Nothing,
        frameworkId = pFrameworkId_,
        destinationAccount = pDestinationAccount_,
        destinationRegion = pDestinationRegion_
      }

-- | An optional comment from the sender about the share request.
startAssessmentFrameworkShare_comment :: Lens.Lens' StartAssessmentFrameworkShare (Prelude.Maybe Prelude.Text)
startAssessmentFrameworkShare_comment = Lens.lens (\StartAssessmentFrameworkShare' {comment} -> comment) (\s@StartAssessmentFrameworkShare' {} a -> s {comment = a} :: StartAssessmentFrameworkShare)

-- | The unique identifier for the custom framework to be shared.
startAssessmentFrameworkShare_frameworkId :: Lens.Lens' StartAssessmentFrameworkShare Prelude.Text
startAssessmentFrameworkShare_frameworkId = Lens.lens (\StartAssessmentFrameworkShare' {frameworkId} -> frameworkId) (\s@StartAssessmentFrameworkShare' {} a -> s {frameworkId = a} :: StartAssessmentFrameworkShare)

-- | The Amazon Web Services account of the recipient.
startAssessmentFrameworkShare_destinationAccount :: Lens.Lens' StartAssessmentFrameworkShare Prelude.Text
startAssessmentFrameworkShare_destinationAccount = Lens.lens (\StartAssessmentFrameworkShare' {destinationAccount} -> destinationAccount) (\s@StartAssessmentFrameworkShare' {} a -> s {destinationAccount = a} :: StartAssessmentFrameworkShare)

-- | The Amazon Web Services Region of the recipient.
startAssessmentFrameworkShare_destinationRegion :: Lens.Lens' StartAssessmentFrameworkShare Prelude.Text
startAssessmentFrameworkShare_destinationRegion = Lens.lens (\StartAssessmentFrameworkShare' {destinationRegion} -> destinationRegion) (\s@StartAssessmentFrameworkShare' {} a -> s {destinationRegion = a} :: StartAssessmentFrameworkShare)

instance
  Core.AWSRequest
    StartAssessmentFrameworkShare
  where
  type
    AWSResponse StartAssessmentFrameworkShare =
      StartAssessmentFrameworkShareResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartAssessmentFrameworkShareResponse'
            Prelude.<$> (x Data..?> "assessmentFrameworkShareRequest")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    StartAssessmentFrameworkShare
  where
  hashWithSalt _salt StartAssessmentFrameworkShare' {..} =
    _salt
      `Prelude.hashWithSalt` comment
      `Prelude.hashWithSalt` frameworkId
      `Prelude.hashWithSalt` destinationAccount
      `Prelude.hashWithSalt` destinationRegion

instance Prelude.NFData StartAssessmentFrameworkShare where
  rnf StartAssessmentFrameworkShare' {..} =
    Prelude.rnf comment
      `Prelude.seq` Prelude.rnf frameworkId
      `Prelude.seq` Prelude.rnf destinationAccount
      `Prelude.seq` Prelude.rnf destinationRegion

instance Data.ToHeaders StartAssessmentFrameworkShare where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartAssessmentFrameworkShare where
  toJSON StartAssessmentFrameworkShare' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("comment" Data..=) Prelude.<$> comment,
            Prelude.Just
              ("destinationAccount" Data..= destinationAccount),
            Prelude.Just
              ("destinationRegion" Data..= destinationRegion)
          ]
      )

instance Data.ToPath StartAssessmentFrameworkShare where
  toPath StartAssessmentFrameworkShare' {..} =
    Prelude.mconcat
      [ "/assessmentFrameworks/",
        Data.toBS frameworkId,
        "/shareRequests"
      ]

instance Data.ToQuery StartAssessmentFrameworkShare where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartAssessmentFrameworkShareResponse' smart constructor.
data StartAssessmentFrameworkShareResponse = StartAssessmentFrameworkShareResponse'
  { -- | The share request that\'s created by the @StartAssessmentFrameworkShare@
    -- API.
    assessmentFrameworkShareRequest :: Prelude.Maybe AssessmentFrameworkShareRequest,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartAssessmentFrameworkShareResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assessmentFrameworkShareRequest', 'startAssessmentFrameworkShareResponse_assessmentFrameworkShareRequest' - The share request that\'s created by the @StartAssessmentFrameworkShare@
-- API.
--
-- 'httpStatus', 'startAssessmentFrameworkShareResponse_httpStatus' - The response's http status code.
newStartAssessmentFrameworkShareResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartAssessmentFrameworkShareResponse
newStartAssessmentFrameworkShareResponse pHttpStatus_ =
  StartAssessmentFrameworkShareResponse'
    { assessmentFrameworkShareRequest =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The share request that\'s created by the @StartAssessmentFrameworkShare@
-- API.
startAssessmentFrameworkShareResponse_assessmentFrameworkShareRequest :: Lens.Lens' StartAssessmentFrameworkShareResponse (Prelude.Maybe AssessmentFrameworkShareRequest)
startAssessmentFrameworkShareResponse_assessmentFrameworkShareRequest = Lens.lens (\StartAssessmentFrameworkShareResponse' {assessmentFrameworkShareRequest} -> assessmentFrameworkShareRequest) (\s@StartAssessmentFrameworkShareResponse' {} a -> s {assessmentFrameworkShareRequest = a} :: StartAssessmentFrameworkShareResponse)

-- | The response's http status code.
startAssessmentFrameworkShareResponse_httpStatus :: Lens.Lens' StartAssessmentFrameworkShareResponse Prelude.Int
startAssessmentFrameworkShareResponse_httpStatus = Lens.lens (\StartAssessmentFrameworkShareResponse' {httpStatus} -> httpStatus) (\s@StartAssessmentFrameworkShareResponse' {} a -> s {httpStatus = a} :: StartAssessmentFrameworkShareResponse)

instance
  Prelude.NFData
    StartAssessmentFrameworkShareResponse
  where
  rnf StartAssessmentFrameworkShareResponse' {..} =
    Prelude.rnf assessmentFrameworkShareRequest
      `Prelude.seq` Prelude.rnf httpStatus
