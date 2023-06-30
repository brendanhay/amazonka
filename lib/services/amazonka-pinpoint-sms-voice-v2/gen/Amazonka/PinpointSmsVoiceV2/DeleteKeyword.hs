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
-- Module      : Amazonka.PinpointSmsVoiceV2.DeleteKeyword
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing keyword from an origination phone number or pool.
--
-- A keyword is a word that you can search for on a particular phone number
-- or pool. It is also a specific word or phrase that an end user can send
-- to your number to elicit a response, such as an informational message or
-- a special offer. When your number receives a message that begins with a
-- keyword, Amazon Pinpoint responds with a customizable message.
--
-- Keywords \"HELP\" and \"STOP\" can\'t be deleted or modified.
module Amazonka.PinpointSmsVoiceV2.DeleteKeyword
  ( -- * Creating a Request
    DeleteKeyword (..),
    newDeleteKeyword,

    -- * Request Lenses
    deleteKeyword_originationIdentity,
    deleteKeyword_keyword,

    -- * Destructuring the Response
    DeleteKeywordResponse (..),
    newDeleteKeywordResponse,

    -- * Response Lenses
    deleteKeywordResponse_keyword,
    deleteKeywordResponse_keywordAction,
    deleteKeywordResponse_keywordMessage,
    deleteKeywordResponse_originationIdentity,
    deleteKeywordResponse_originationIdentityArn,
    deleteKeywordResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointSmsVoiceV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteKeyword' smart constructor.
data DeleteKeyword = DeleteKeyword'
  { -- | The origination identity to use such as a PhoneNumberId, PhoneNumberArn,
    -- PoolId or PoolArn. You can use DescribePhoneNumbers to find the values
    -- for PhoneNumberId and PhoneNumberArn and DescribePools to find the
    -- values of PoolId and PoolArn.
    originationIdentity :: Prelude.Text,
    -- | The keyword to delete.
    keyword :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteKeyword' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'originationIdentity', 'deleteKeyword_originationIdentity' - The origination identity to use such as a PhoneNumberId, PhoneNumberArn,
-- PoolId or PoolArn. You can use DescribePhoneNumbers to find the values
-- for PhoneNumberId and PhoneNumberArn and DescribePools to find the
-- values of PoolId and PoolArn.
--
-- 'keyword', 'deleteKeyword_keyword' - The keyword to delete.
newDeleteKeyword ::
  -- | 'originationIdentity'
  Prelude.Text ->
  -- | 'keyword'
  Prelude.Text ->
  DeleteKeyword
newDeleteKeyword pOriginationIdentity_ pKeyword_ =
  DeleteKeyword'
    { originationIdentity =
        pOriginationIdentity_,
      keyword = pKeyword_
    }

-- | The origination identity to use such as a PhoneNumberId, PhoneNumberArn,
-- PoolId or PoolArn. You can use DescribePhoneNumbers to find the values
-- for PhoneNumberId and PhoneNumberArn and DescribePools to find the
-- values of PoolId and PoolArn.
deleteKeyword_originationIdentity :: Lens.Lens' DeleteKeyword Prelude.Text
deleteKeyword_originationIdentity = Lens.lens (\DeleteKeyword' {originationIdentity} -> originationIdentity) (\s@DeleteKeyword' {} a -> s {originationIdentity = a} :: DeleteKeyword)

-- | The keyword to delete.
deleteKeyword_keyword :: Lens.Lens' DeleteKeyword Prelude.Text
deleteKeyword_keyword = Lens.lens (\DeleteKeyword' {keyword} -> keyword) (\s@DeleteKeyword' {} a -> s {keyword = a} :: DeleteKeyword)

instance Core.AWSRequest DeleteKeyword where
  type
    AWSResponse DeleteKeyword =
      DeleteKeywordResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteKeywordResponse'
            Prelude.<$> (x Data..?> "Keyword")
            Prelude.<*> (x Data..?> "KeywordAction")
            Prelude.<*> (x Data..?> "KeywordMessage")
            Prelude.<*> (x Data..?> "OriginationIdentity")
            Prelude.<*> (x Data..?> "OriginationIdentityArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteKeyword where
  hashWithSalt _salt DeleteKeyword' {..} =
    _salt
      `Prelude.hashWithSalt` originationIdentity
      `Prelude.hashWithSalt` keyword

instance Prelude.NFData DeleteKeyword where
  rnf DeleteKeyword' {..} =
    Prelude.rnf originationIdentity
      `Prelude.seq` Prelude.rnf keyword

instance Data.ToHeaders DeleteKeyword where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PinpointSMSVoiceV2.DeleteKeyword" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteKeyword where
  toJSON DeleteKeyword' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("OriginationIdentity" Data..= originationIdentity),
            Prelude.Just ("Keyword" Data..= keyword)
          ]
      )

instance Data.ToPath DeleteKeyword where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteKeyword where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteKeywordResponse' smart constructor.
data DeleteKeywordResponse = DeleteKeywordResponse'
  { -- | The keyword that was deleted.
    keyword :: Prelude.Maybe Prelude.Text,
    -- | The action that was associated with the deleted keyword.
    keywordAction :: Prelude.Maybe KeywordAction,
    -- | The message that was associated with the deleted keyword.
    keywordMessage :: Prelude.Maybe Prelude.Text,
    -- | The PhoneNumberId or PoolId that the keyword was associated with.
    originationIdentity :: Prelude.Maybe Prelude.Text,
    -- | The PhoneNumberArn or PoolArn that the keyword was associated with.
    originationIdentityArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteKeywordResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyword', 'deleteKeywordResponse_keyword' - The keyword that was deleted.
--
-- 'keywordAction', 'deleteKeywordResponse_keywordAction' - The action that was associated with the deleted keyword.
--
-- 'keywordMessage', 'deleteKeywordResponse_keywordMessage' - The message that was associated with the deleted keyword.
--
-- 'originationIdentity', 'deleteKeywordResponse_originationIdentity' - The PhoneNumberId or PoolId that the keyword was associated with.
--
-- 'originationIdentityArn', 'deleteKeywordResponse_originationIdentityArn' - The PhoneNumberArn or PoolArn that the keyword was associated with.
--
-- 'httpStatus', 'deleteKeywordResponse_httpStatus' - The response's http status code.
newDeleteKeywordResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteKeywordResponse
newDeleteKeywordResponse pHttpStatus_ =
  DeleteKeywordResponse'
    { keyword = Prelude.Nothing,
      keywordAction = Prelude.Nothing,
      keywordMessage = Prelude.Nothing,
      originationIdentity = Prelude.Nothing,
      originationIdentityArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The keyword that was deleted.
deleteKeywordResponse_keyword :: Lens.Lens' DeleteKeywordResponse (Prelude.Maybe Prelude.Text)
deleteKeywordResponse_keyword = Lens.lens (\DeleteKeywordResponse' {keyword} -> keyword) (\s@DeleteKeywordResponse' {} a -> s {keyword = a} :: DeleteKeywordResponse)

-- | The action that was associated with the deleted keyword.
deleteKeywordResponse_keywordAction :: Lens.Lens' DeleteKeywordResponse (Prelude.Maybe KeywordAction)
deleteKeywordResponse_keywordAction = Lens.lens (\DeleteKeywordResponse' {keywordAction} -> keywordAction) (\s@DeleteKeywordResponse' {} a -> s {keywordAction = a} :: DeleteKeywordResponse)

-- | The message that was associated with the deleted keyword.
deleteKeywordResponse_keywordMessage :: Lens.Lens' DeleteKeywordResponse (Prelude.Maybe Prelude.Text)
deleteKeywordResponse_keywordMessage = Lens.lens (\DeleteKeywordResponse' {keywordMessage} -> keywordMessage) (\s@DeleteKeywordResponse' {} a -> s {keywordMessage = a} :: DeleteKeywordResponse)

-- | The PhoneNumberId or PoolId that the keyword was associated with.
deleteKeywordResponse_originationIdentity :: Lens.Lens' DeleteKeywordResponse (Prelude.Maybe Prelude.Text)
deleteKeywordResponse_originationIdentity = Lens.lens (\DeleteKeywordResponse' {originationIdentity} -> originationIdentity) (\s@DeleteKeywordResponse' {} a -> s {originationIdentity = a} :: DeleteKeywordResponse)

-- | The PhoneNumberArn or PoolArn that the keyword was associated with.
deleteKeywordResponse_originationIdentityArn :: Lens.Lens' DeleteKeywordResponse (Prelude.Maybe Prelude.Text)
deleteKeywordResponse_originationIdentityArn = Lens.lens (\DeleteKeywordResponse' {originationIdentityArn} -> originationIdentityArn) (\s@DeleteKeywordResponse' {} a -> s {originationIdentityArn = a} :: DeleteKeywordResponse)

-- | The response's http status code.
deleteKeywordResponse_httpStatus :: Lens.Lens' DeleteKeywordResponse Prelude.Int
deleteKeywordResponse_httpStatus = Lens.lens (\DeleteKeywordResponse' {httpStatus} -> httpStatus) (\s@DeleteKeywordResponse' {} a -> s {httpStatus = a} :: DeleteKeywordResponse)

instance Prelude.NFData DeleteKeywordResponse where
  rnf DeleteKeywordResponse' {..} =
    Prelude.rnf keyword
      `Prelude.seq` Prelude.rnf keywordAction
      `Prelude.seq` Prelude.rnf keywordMessage
      `Prelude.seq` Prelude.rnf originationIdentity
      `Prelude.seq` Prelude.rnf originationIdentityArn
      `Prelude.seq` Prelude.rnf httpStatus
