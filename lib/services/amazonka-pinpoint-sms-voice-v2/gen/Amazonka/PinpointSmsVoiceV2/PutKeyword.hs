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
-- Module      : Amazonka.PinpointSmsVoiceV2.PutKeyword
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a keyword configuration on an origination phone
-- number or pool.
--
-- A keyword is a word that you can search for on a particular phone number
-- or pool. It is also a specific word or phrase that an end user can send
-- to your number to elicit a response, such as an informational message or
-- a special offer. When your number receives a message that begins with a
-- keyword, Amazon Pinpoint responds with a customizable message.
--
-- If you specify a keyword that isn\'t valid, an Error is returned.
module Amazonka.PinpointSmsVoiceV2.PutKeyword
  ( -- * Creating a Request
    PutKeyword (..),
    newPutKeyword,

    -- * Request Lenses
    putKeyword_keywordAction,
    putKeyword_originationIdentity,
    putKeyword_keyword,
    putKeyword_keywordMessage,

    -- * Destructuring the Response
    PutKeywordResponse (..),
    newPutKeywordResponse,

    -- * Response Lenses
    putKeywordResponse_originationIdentity,
    putKeywordResponse_keywordAction,
    putKeywordResponse_originationIdentityArn,
    putKeywordResponse_keyword,
    putKeywordResponse_keywordMessage,
    putKeywordResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.PinpointSmsVoiceV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutKeyword' smart constructor.
data PutKeyword = PutKeyword'
  { -- | The action to perform for the new keyword when it is received.
    keywordAction :: Prelude.Maybe KeywordAction,
    -- | The origination identity to use such as a PhoneNumberId, PhoneNumberArn,
    -- SenderId or SenderIdArn. You can use DescribePhoneNumbers get the values
    -- for PhoneNumberId and PhoneNumberArn while DescribeSenderIds can be used
    -- to get the values for SenderId and SenderIdArn.
    originationIdentity :: Prelude.Text,
    -- | The new keyword to add.
    keyword :: Prelude.Text,
    -- | The message associated with the keyword.
    --
    -- -   AUTOMATIC_RESPONSE: A message is sent to the recipient.
    --
    -- -   OPT_OUT: Keeps the recipient from receiving future messages.
    --
    -- -   OPT_IN: The recipient wants to receive future messages.
    keywordMessage :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutKeyword' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keywordAction', 'putKeyword_keywordAction' - The action to perform for the new keyword when it is received.
--
-- 'originationIdentity', 'putKeyword_originationIdentity' - The origination identity to use such as a PhoneNumberId, PhoneNumberArn,
-- SenderId or SenderIdArn. You can use DescribePhoneNumbers get the values
-- for PhoneNumberId and PhoneNumberArn while DescribeSenderIds can be used
-- to get the values for SenderId and SenderIdArn.
--
-- 'keyword', 'putKeyword_keyword' - The new keyword to add.
--
-- 'keywordMessage', 'putKeyword_keywordMessage' - The message associated with the keyword.
--
-- -   AUTOMATIC_RESPONSE: A message is sent to the recipient.
--
-- -   OPT_OUT: Keeps the recipient from receiving future messages.
--
-- -   OPT_IN: The recipient wants to receive future messages.
newPutKeyword ::
  -- | 'originationIdentity'
  Prelude.Text ->
  -- | 'keyword'
  Prelude.Text ->
  -- | 'keywordMessage'
  Prelude.Text ->
  PutKeyword
newPutKeyword
  pOriginationIdentity_
  pKeyword_
  pKeywordMessage_ =
    PutKeyword'
      { keywordAction = Prelude.Nothing,
        originationIdentity = pOriginationIdentity_,
        keyword = pKeyword_,
        keywordMessage = pKeywordMessage_
      }

-- | The action to perform for the new keyword when it is received.
putKeyword_keywordAction :: Lens.Lens' PutKeyword (Prelude.Maybe KeywordAction)
putKeyword_keywordAction = Lens.lens (\PutKeyword' {keywordAction} -> keywordAction) (\s@PutKeyword' {} a -> s {keywordAction = a} :: PutKeyword)

-- | The origination identity to use such as a PhoneNumberId, PhoneNumberArn,
-- SenderId or SenderIdArn. You can use DescribePhoneNumbers get the values
-- for PhoneNumberId and PhoneNumberArn while DescribeSenderIds can be used
-- to get the values for SenderId and SenderIdArn.
putKeyword_originationIdentity :: Lens.Lens' PutKeyword Prelude.Text
putKeyword_originationIdentity = Lens.lens (\PutKeyword' {originationIdentity} -> originationIdentity) (\s@PutKeyword' {} a -> s {originationIdentity = a} :: PutKeyword)

-- | The new keyword to add.
putKeyword_keyword :: Lens.Lens' PutKeyword Prelude.Text
putKeyword_keyword = Lens.lens (\PutKeyword' {keyword} -> keyword) (\s@PutKeyword' {} a -> s {keyword = a} :: PutKeyword)

-- | The message associated with the keyword.
--
-- -   AUTOMATIC_RESPONSE: A message is sent to the recipient.
--
-- -   OPT_OUT: Keeps the recipient from receiving future messages.
--
-- -   OPT_IN: The recipient wants to receive future messages.
putKeyword_keywordMessage :: Lens.Lens' PutKeyword Prelude.Text
putKeyword_keywordMessage = Lens.lens (\PutKeyword' {keywordMessage} -> keywordMessage) (\s@PutKeyword' {} a -> s {keywordMessage = a} :: PutKeyword)

instance Core.AWSRequest PutKeyword where
  type AWSResponse PutKeyword = PutKeywordResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutKeywordResponse'
            Prelude.<$> (x Core..?> "OriginationIdentity")
            Prelude.<*> (x Core..?> "KeywordAction")
            Prelude.<*> (x Core..?> "OriginationIdentityArn")
            Prelude.<*> (x Core..?> "Keyword")
            Prelude.<*> (x Core..?> "KeywordMessage")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutKeyword where
  hashWithSalt _salt PutKeyword' {..} =
    _salt `Prelude.hashWithSalt` keywordAction
      `Prelude.hashWithSalt` originationIdentity
      `Prelude.hashWithSalt` keyword
      `Prelude.hashWithSalt` keywordMessage

instance Prelude.NFData PutKeyword where
  rnf PutKeyword' {..} =
    Prelude.rnf keywordAction
      `Prelude.seq` Prelude.rnf originationIdentity
      `Prelude.seq` Prelude.rnf keyword
      `Prelude.seq` Prelude.rnf keywordMessage

instance Core.ToHeaders PutKeyword where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "PinpointSMSVoiceV2.PutKeyword" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON PutKeyword where
  toJSON PutKeyword' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("KeywordAction" Core..=) Prelude.<$> keywordAction,
            Prelude.Just
              ("OriginationIdentity" Core..= originationIdentity),
            Prelude.Just ("Keyword" Core..= keyword),
            Prelude.Just
              ("KeywordMessage" Core..= keywordMessage)
          ]
      )

instance Core.ToPath PutKeyword where
  toPath = Prelude.const "/"

instance Core.ToQuery PutKeyword where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutKeywordResponse' smart constructor.
data PutKeywordResponse = PutKeywordResponse'
  { -- | The PhoneNumberId or PoolId that the keyword was associated with.
    originationIdentity :: Prelude.Maybe Prelude.Text,
    -- | The action to perform when the keyword is used.
    keywordAction :: Prelude.Maybe KeywordAction,
    -- | The PhoneNumberArn or PoolArn that the keyword was associated with.
    originationIdentityArn :: Prelude.Maybe Prelude.Text,
    -- | The keyword that was added.
    keyword :: Prelude.Maybe Prelude.Text,
    -- | The message associated with the keyword.
    keywordMessage :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutKeywordResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'originationIdentity', 'putKeywordResponse_originationIdentity' - The PhoneNumberId or PoolId that the keyword was associated with.
--
-- 'keywordAction', 'putKeywordResponse_keywordAction' - The action to perform when the keyword is used.
--
-- 'originationIdentityArn', 'putKeywordResponse_originationIdentityArn' - The PhoneNumberArn or PoolArn that the keyword was associated with.
--
-- 'keyword', 'putKeywordResponse_keyword' - The keyword that was added.
--
-- 'keywordMessage', 'putKeywordResponse_keywordMessage' - The message associated with the keyword.
--
-- 'httpStatus', 'putKeywordResponse_httpStatus' - The response's http status code.
newPutKeywordResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutKeywordResponse
newPutKeywordResponse pHttpStatus_ =
  PutKeywordResponse'
    { originationIdentity =
        Prelude.Nothing,
      keywordAction = Prelude.Nothing,
      originationIdentityArn = Prelude.Nothing,
      keyword = Prelude.Nothing,
      keywordMessage = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The PhoneNumberId or PoolId that the keyword was associated with.
putKeywordResponse_originationIdentity :: Lens.Lens' PutKeywordResponse (Prelude.Maybe Prelude.Text)
putKeywordResponse_originationIdentity = Lens.lens (\PutKeywordResponse' {originationIdentity} -> originationIdentity) (\s@PutKeywordResponse' {} a -> s {originationIdentity = a} :: PutKeywordResponse)

-- | The action to perform when the keyword is used.
putKeywordResponse_keywordAction :: Lens.Lens' PutKeywordResponse (Prelude.Maybe KeywordAction)
putKeywordResponse_keywordAction = Lens.lens (\PutKeywordResponse' {keywordAction} -> keywordAction) (\s@PutKeywordResponse' {} a -> s {keywordAction = a} :: PutKeywordResponse)

-- | The PhoneNumberArn or PoolArn that the keyword was associated with.
putKeywordResponse_originationIdentityArn :: Lens.Lens' PutKeywordResponse (Prelude.Maybe Prelude.Text)
putKeywordResponse_originationIdentityArn = Lens.lens (\PutKeywordResponse' {originationIdentityArn} -> originationIdentityArn) (\s@PutKeywordResponse' {} a -> s {originationIdentityArn = a} :: PutKeywordResponse)

-- | The keyword that was added.
putKeywordResponse_keyword :: Lens.Lens' PutKeywordResponse (Prelude.Maybe Prelude.Text)
putKeywordResponse_keyword = Lens.lens (\PutKeywordResponse' {keyword} -> keyword) (\s@PutKeywordResponse' {} a -> s {keyword = a} :: PutKeywordResponse)

-- | The message associated with the keyword.
putKeywordResponse_keywordMessage :: Lens.Lens' PutKeywordResponse (Prelude.Maybe Prelude.Text)
putKeywordResponse_keywordMessage = Lens.lens (\PutKeywordResponse' {keywordMessage} -> keywordMessage) (\s@PutKeywordResponse' {} a -> s {keywordMessage = a} :: PutKeywordResponse)

-- | The response's http status code.
putKeywordResponse_httpStatus :: Lens.Lens' PutKeywordResponse Prelude.Int
putKeywordResponse_httpStatus = Lens.lens (\PutKeywordResponse' {httpStatus} -> httpStatus) (\s@PutKeywordResponse' {} a -> s {httpStatus = a} :: PutKeywordResponse)

instance Prelude.NFData PutKeywordResponse where
  rnf PutKeywordResponse' {..} =
    Prelude.rnf originationIdentity
      `Prelude.seq` Prelude.rnf keywordAction
      `Prelude.seq` Prelude.rnf originationIdentityArn
      `Prelude.seq` Prelude.rnf keyword
      `Prelude.seq` Prelude.rnf keywordMessage
      `Prelude.seq` Prelude.rnf httpStatus
