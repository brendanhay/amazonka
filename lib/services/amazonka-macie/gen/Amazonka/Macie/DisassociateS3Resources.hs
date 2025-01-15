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
-- Module      : Amazonka.Macie.DisassociateS3Resources
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- (Discontinued) Removes specified S3 resources from being monitored by
-- Amazon Macie Classic. If @memberAccountId@ isn\'t specified, the action
-- removes specified S3 resources from Macie Classic for the current Macie
-- Classic administrator account. If @memberAccountId@ is specified, the
-- action removes specified S3 resources from Macie Classic for the
-- specified member account.
module Amazonka.Macie.DisassociateS3Resources
  ( -- * Creating a Request
    DisassociateS3Resources (..),
    newDisassociateS3Resources,

    -- * Request Lenses
    disassociateS3Resources_memberAccountId,
    disassociateS3Resources_associatedS3Resources,

    -- * Destructuring the Response
    DisassociateS3ResourcesResponse (..),
    newDisassociateS3ResourcesResponse,

    -- * Response Lenses
    disassociateS3ResourcesResponse_failedS3Resources,
    disassociateS3ResourcesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Macie.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateS3Resources' smart constructor.
data DisassociateS3Resources = DisassociateS3Resources'
  { -- | (Discontinued) The ID of the Amazon Macie Classic member account whose
    -- resources you want to remove from being monitored by Macie Classic.
    memberAccountId :: Prelude.Maybe Prelude.Text,
    -- | (Discontinued) The S3 resources (buckets or prefixes) that you want to
    -- remove from being monitored and classified by Amazon Macie Classic.
    associatedS3Resources :: [S3Resource]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateS3Resources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'memberAccountId', 'disassociateS3Resources_memberAccountId' - (Discontinued) The ID of the Amazon Macie Classic member account whose
-- resources you want to remove from being monitored by Macie Classic.
--
-- 'associatedS3Resources', 'disassociateS3Resources_associatedS3Resources' - (Discontinued) The S3 resources (buckets or prefixes) that you want to
-- remove from being monitored and classified by Amazon Macie Classic.
newDisassociateS3Resources ::
  DisassociateS3Resources
newDisassociateS3Resources =
  DisassociateS3Resources'
    { memberAccountId =
        Prelude.Nothing,
      associatedS3Resources = Prelude.mempty
    }

-- | (Discontinued) The ID of the Amazon Macie Classic member account whose
-- resources you want to remove from being monitored by Macie Classic.
disassociateS3Resources_memberAccountId :: Lens.Lens' DisassociateS3Resources (Prelude.Maybe Prelude.Text)
disassociateS3Resources_memberAccountId = Lens.lens (\DisassociateS3Resources' {memberAccountId} -> memberAccountId) (\s@DisassociateS3Resources' {} a -> s {memberAccountId = a} :: DisassociateS3Resources)

-- | (Discontinued) The S3 resources (buckets or prefixes) that you want to
-- remove from being monitored and classified by Amazon Macie Classic.
disassociateS3Resources_associatedS3Resources :: Lens.Lens' DisassociateS3Resources [S3Resource]
disassociateS3Resources_associatedS3Resources = Lens.lens (\DisassociateS3Resources' {associatedS3Resources} -> associatedS3Resources) (\s@DisassociateS3Resources' {} a -> s {associatedS3Resources = a} :: DisassociateS3Resources) Prelude.. Lens.coerced

instance Core.AWSRequest DisassociateS3Resources where
  type
    AWSResponse DisassociateS3Resources =
      DisassociateS3ResourcesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DisassociateS3ResourcesResponse'
            Prelude.<$> ( x
                            Data..?> "failedS3Resources"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisassociateS3Resources where
  hashWithSalt _salt DisassociateS3Resources' {..} =
    _salt
      `Prelude.hashWithSalt` memberAccountId
      `Prelude.hashWithSalt` associatedS3Resources

instance Prelude.NFData DisassociateS3Resources where
  rnf DisassociateS3Resources' {..} =
    Prelude.rnf memberAccountId `Prelude.seq`
      Prelude.rnf associatedS3Resources

instance Data.ToHeaders DisassociateS3Resources where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "MacieService.DisassociateS3Resources" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisassociateS3Resources where
  toJSON DisassociateS3Resources' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("memberAccountId" Data..=)
              Prelude.<$> memberAccountId,
            Prelude.Just
              ( "associatedS3Resources"
                  Data..= associatedS3Resources
              )
          ]
      )

instance Data.ToPath DisassociateS3Resources where
  toPath = Prelude.const "/"

instance Data.ToQuery DisassociateS3Resources where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateS3ResourcesResponse' smart constructor.
data DisassociateS3ResourcesResponse = DisassociateS3ResourcesResponse'
  { -- | (Discontinued) S3 resources that couldn\'t be removed from being
    -- monitored and classified by Amazon Macie Classic. An error code and an
    -- error message are provided for each failed item.
    failedS3Resources :: Prelude.Maybe [FailedS3Resource],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateS3ResourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failedS3Resources', 'disassociateS3ResourcesResponse_failedS3Resources' - (Discontinued) S3 resources that couldn\'t be removed from being
-- monitored and classified by Amazon Macie Classic. An error code and an
-- error message are provided for each failed item.
--
-- 'httpStatus', 'disassociateS3ResourcesResponse_httpStatus' - The response's http status code.
newDisassociateS3ResourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateS3ResourcesResponse
newDisassociateS3ResourcesResponse pHttpStatus_ =
  DisassociateS3ResourcesResponse'
    { failedS3Resources =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | (Discontinued) S3 resources that couldn\'t be removed from being
-- monitored and classified by Amazon Macie Classic. An error code and an
-- error message are provided for each failed item.
disassociateS3ResourcesResponse_failedS3Resources :: Lens.Lens' DisassociateS3ResourcesResponse (Prelude.Maybe [FailedS3Resource])
disassociateS3ResourcesResponse_failedS3Resources = Lens.lens (\DisassociateS3ResourcesResponse' {failedS3Resources} -> failedS3Resources) (\s@DisassociateS3ResourcesResponse' {} a -> s {failedS3Resources = a} :: DisassociateS3ResourcesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
disassociateS3ResourcesResponse_httpStatus :: Lens.Lens' DisassociateS3ResourcesResponse Prelude.Int
disassociateS3ResourcesResponse_httpStatus = Lens.lens (\DisassociateS3ResourcesResponse' {httpStatus} -> httpStatus) (\s@DisassociateS3ResourcesResponse' {} a -> s {httpStatus = a} :: DisassociateS3ResourcesResponse)

instance
  Prelude.NFData
    DisassociateS3ResourcesResponse
  where
  rnf DisassociateS3ResourcesResponse' {..} =
    Prelude.rnf failedS3Resources `Prelude.seq`
      Prelude.rnf httpStatus
