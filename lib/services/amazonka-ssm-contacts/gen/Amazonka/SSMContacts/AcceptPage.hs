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
-- Module      : Amazonka.SSMContacts.AcceptPage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used to acknowledge an engagement to a contact channel during an
-- incident.
module Amazonka.SSMContacts.AcceptPage
  ( -- * Creating a Request
    AcceptPage (..),
    newAcceptPage,

    -- * Request Lenses
    acceptPage_acceptCodeValidation,
    acceptPage_contactChannelId,
    acceptPage_note,
    acceptPage_pageId,
    acceptPage_acceptType,
    acceptPage_acceptCode,

    -- * Destructuring the Response
    AcceptPageResponse (..),
    newAcceptPageResponse,

    -- * Response Lenses
    acceptPageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMContacts.Types

-- | /See:/ 'newAcceptPage' smart constructor.
data AcceptPage = AcceptPage'
  { -- | An optional field that Incident Manager uses to @ENFORCE@ @AcceptCode@
    -- validation when acknowledging an page. Acknowledgement can occur by
    -- replying to a page, or when entering the AcceptCode in the console.
    -- Enforcing AcceptCode validation causes Incident Manager to verify that
    -- the code entered by the user matches the code sent by Incident Manager
    -- with the page.
    --
    -- Incident Manager can also @IGNORE@ @AcceptCode@ validation. Ignoring
    -- @AcceptCode@ validation causes Incident Manager to accept any value
    -- entered for the @AcceptCode@.
    acceptCodeValidation :: Prelude.Maybe AcceptCodeValidation,
    -- | The ARN of the contact channel.
    contactChannelId :: Prelude.Maybe Prelude.Text,
    -- | Information provided by the user when the user acknowledges the page.
    note :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the engagement to a contact channel.
    pageId :: Prelude.Text,
    -- | The type indicates if the page was @DELIVERED@ or @READ@.
    acceptType :: AcceptType,
    -- | The accept code is a 6-digit code used to acknowledge the page.
    acceptCode :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AcceptPage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceptCodeValidation', 'acceptPage_acceptCodeValidation' - An optional field that Incident Manager uses to @ENFORCE@ @AcceptCode@
-- validation when acknowledging an page. Acknowledgement can occur by
-- replying to a page, or when entering the AcceptCode in the console.
-- Enforcing AcceptCode validation causes Incident Manager to verify that
-- the code entered by the user matches the code sent by Incident Manager
-- with the page.
--
-- Incident Manager can also @IGNORE@ @AcceptCode@ validation. Ignoring
-- @AcceptCode@ validation causes Incident Manager to accept any value
-- entered for the @AcceptCode@.
--
-- 'contactChannelId', 'acceptPage_contactChannelId' - The ARN of the contact channel.
--
-- 'note', 'acceptPage_note' - Information provided by the user when the user acknowledges the page.
--
-- 'pageId', 'acceptPage_pageId' - The Amazon Resource Name (ARN) of the engagement to a contact channel.
--
-- 'acceptType', 'acceptPage_acceptType' - The type indicates if the page was @DELIVERED@ or @READ@.
--
-- 'acceptCode', 'acceptPage_acceptCode' - The accept code is a 6-digit code used to acknowledge the page.
newAcceptPage ::
  -- | 'pageId'
  Prelude.Text ->
  -- | 'acceptType'
  AcceptType ->
  -- | 'acceptCode'
  Prelude.Text ->
  AcceptPage
newAcceptPage pPageId_ pAcceptType_ pAcceptCode_ =
  AcceptPage'
    { acceptCodeValidation = Prelude.Nothing,
      contactChannelId = Prelude.Nothing,
      note = Prelude.Nothing,
      pageId = pPageId_,
      acceptType = pAcceptType_,
      acceptCode = pAcceptCode_
    }

-- | An optional field that Incident Manager uses to @ENFORCE@ @AcceptCode@
-- validation when acknowledging an page. Acknowledgement can occur by
-- replying to a page, or when entering the AcceptCode in the console.
-- Enforcing AcceptCode validation causes Incident Manager to verify that
-- the code entered by the user matches the code sent by Incident Manager
-- with the page.
--
-- Incident Manager can also @IGNORE@ @AcceptCode@ validation. Ignoring
-- @AcceptCode@ validation causes Incident Manager to accept any value
-- entered for the @AcceptCode@.
acceptPage_acceptCodeValidation :: Lens.Lens' AcceptPage (Prelude.Maybe AcceptCodeValidation)
acceptPage_acceptCodeValidation = Lens.lens (\AcceptPage' {acceptCodeValidation} -> acceptCodeValidation) (\s@AcceptPage' {} a -> s {acceptCodeValidation = a} :: AcceptPage)

-- | The ARN of the contact channel.
acceptPage_contactChannelId :: Lens.Lens' AcceptPage (Prelude.Maybe Prelude.Text)
acceptPage_contactChannelId = Lens.lens (\AcceptPage' {contactChannelId} -> contactChannelId) (\s@AcceptPage' {} a -> s {contactChannelId = a} :: AcceptPage)

-- | Information provided by the user when the user acknowledges the page.
acceptPage_note :: Lens.Lens' AcceptPage (Prelude.Maybe Prelude.Text)
acceptPage_note = Lens.lens (\AcceptPage' {note} -> note) (\s@AcceptPage' {} a -> s {note = a} :: AcceptPage)

-- | The Amazon Resource Name (ARN) of the engagement to a contact channel.
acceptPage_pageId :: Lens.Lens' AcceptPage Prelude.Text
acceptPage_pageId = Lens.lens (\AcceptPage' {pageId} -> pageId) (\s@AcceptPage' {} a -> s {pageId = a} :: AcceptPage)

-- | The type indicates if the page was @DELIVERED@ or @READ@.
acceptPage_acceptType :: Lens.Lens' AcceptPage AcceptType
acceptPage_acceptType = Lens.lens (\AcceptPage' {acceptType} -> acceptType) (\s@AcceptPage' {} a -> s {acceptType = a} :: AcceptPage)

-- | The accept code is a 6-digit code used to acknowledge the page.
acceptPage_acceptCode :: Lens.Lens' AcceptPage Prelude.Text
acceptPage_acceptCode = Lens.lens (\AcceptPage' {acceptCode} -> acceptCode) (\s@AcceptPage' {} a -> s {acceptCode = a} :: AcceptPage)

instance Core.AWSRequest AcceptPage where
  type AWSResponse AcceptPage = AcceptPageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          AcceptPageResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AcceptPage where
  hashWithSalt _salt AcceptPage' {..} =
    _salt `Prelude.hashWithSalt` acceptCodeValidation
      `Prelude.hashWithSalt` contactChannelId
      `Prelude.hashWithSalt` note
      `Prelude.hashWithSalt` pageId
      `Prelude.hashWithSalt` acceptType
      `Prelude.hashWithSalt` acceptCode

instance Prelude.NFData AcceptPage where
  rnf AcceptPage' {..} =
    Prelude.rnf acceptCodeValidation
      `Prelude.seq` Prelude.rnf contactChannelId
      `Prelude.seq` Prelude.rnf note
      `Prelude.seq` Prelude.rnf pageId
      `Prelude.seq` Prelude.rnf acceptType
      `Prelude.seq` Prelude.rnf acceptCode

instance Data.ToHeaders AcceptPage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SSMContacts.AcceptPage" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AcceptPage where
  toJSON AcceptPage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AcceptCodeValidation" Data..=)
              Prelude.<$> acceptCodeValidation,
            ("ContactChannelId" Data..=)
              Prelude.<$> contactChannelId,
            ("Note" Data..=) Prelude.<$> note,
            Prelude.Just ("PageId" Data..= pageId),
            Prelude.Just ("AcceptType" Data..= acceptType),
            Prelude.Just ("AcceptCode" Data..= acceptCode)
          ]
      )

instance Data.ToPath AcceptPage where
  toPath = Prelude.const "/"

instance Data.ToQuery AcceptPage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAcceptPageResponse' smart constructor.
data AcceptPageResponse = AcceptPageResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AcceptPageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'acceptPageResponse_httpStatus' - The response's http status code.
newAcceptPageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AcceptPageResponse
newAcceptPageResponse pHttpStatus_ =
  AcceptPageResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
acceptPageResponse_httpStatus :: Lens.Lens' AcceptPageResponse Prelude.Int
acceptPageResponse_httpStatus = Lens.lens (\AcceptPageResponse' {httpStatus} -> httpStatus) (\s@AcceptPageResponse' {} a -> s {httpStatus = a} :: AcceptPageResponse)

instance Prelude.NFData AcceptPageResponse where
  rnf AcceptPageResponse' {..} = Prelude.rnf httpStatus
