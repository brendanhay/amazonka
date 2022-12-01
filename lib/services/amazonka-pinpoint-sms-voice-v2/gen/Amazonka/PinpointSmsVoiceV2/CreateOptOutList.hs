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
-- Module      : Amazonka.PinpointSmsVoiceV2.CreateOptOutList
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new opt-out list.
--
-- If the opt-out list name already exists, an Error is returned.
--
-- An opt-out list is a list of phone numbers that are opted out, meaning
-- you can\'t send SMS or voice messages to them. If end user replies with
-- the keyword \"STOP,\" an entry for the phone number is added to the
-- opt-out list. In addition to STOP, your recipients can use any supported
-- opt-out keyword, such as CANCEL or OPTOUT. For a list of supported
-- opt-out keywords, see
-- <https://docs.aws.amazon.com/pinpoint/latest/userguide/channels-sms-manage.html#channels-sms-manage-optout SMS opt out>
-- in the /Amazon Pinpoint User Guide/.
module Amazonka.PinpointSmsVoiceV2.CreateOptOutList
  ( -- * Creating a Request
    CreateOptOutList (..),
    newCreateOptOutList,

    -- * Request Lenses
    createOptOutList_tags,
    createOptOutList_clientToken,
    createOptOutList_optOutListName,

    -- * Destructuring the Response
    CreateOptOutListResponse (..),
    newCreateOptOutListResponse,

    -- * Response Lenses
    createOptOutListResponse_tags,
    createOptOutListResponse_optOutListArn,
    createOptOutListResponse_createdTimestamp,
    createOptOutListResponse_optOutListName,
    createOptOutListResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.PinpointSmsVoiceV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateOptOutList' smart constructor.
data CreateOptOutList = CreateOptOutList'
  { -- | An array of tags (key and value pairs) to associate with the new
    -- OptOutList.
    tags :: Prelude.Maybe [Tag],
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. If you don\'t specify a client token, a
    -- randomly generated token is used for the request to ensure idempotency.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the new OptOutList.
    optOutListName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateOptOutList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createOptOutList_tags' - An array of tags (key and value pairs) to associate with the new
-- OptOutList.
--
-- 'clientToken', 'createOptOutList_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you don\'t specify a client token, a
-- randomly generated token is used for the request to ensure idempotency.
--
-- 'optOutListName', 'createOptOutList_optOutListName' - The name of the new OptOutList.
newCreateOptOutList ::
  -- | 'optOutListName'
  Prelude.Text ->
  CreateOptOutList
newCreateOptOutList pOptOutListName_ =
  CreateOptOutList'
    { tags = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      optOutListName = pOptOutListName_
    }

-- | An array of tags (key and value pairs) to associate with the new
-- OptOutList.
createOptOutList_tags :: Lens.Lens' CreateOptOutList (Prelude.Maybe [Tag])
createOptOutList_tags = Lens.lens (\CreateOptOutList' {tags} -> tags) (\s@CreateOptOutList' {} a -> s {tags = a} :: CreateOptOutList) Prelude.. Lens.mapping Lens.coerced

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you don\'t specify a client token, a
-- randomly generated token is used for the request to ensure idempotency.
createOptOutList_clientToken :: Lens.Lens' CreateOptOutList (Prelude.Maybe Prelude.Text)
createOptOutList_clientToken = Lens.lens (\CreateOptOutList' {clientToken} -> clientToken) (\s@CreateOptOutList' {} a -> s {clientToken = a} :: CreateOptOutList)

-- | The name of the new OptOutList.
createOptOutList_optOutListName :: Lens.Lens' CreateOptOutList Prelude.Text
createOptOutList_optOutListName = Lens.lens (\CreateOptOutList' {optOutListName} -> optOutListName) (\s@CreateOptOutList' {} a -> s {optOutListName = a} :: CreateOptOutList)

instance Core.AWSRequest CreateOptOutList where
  type
    AWSResponse CreateOptOutList =
      CreateOptOutListResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateOptOutListResponse'
            Prelude.<$> (x Core..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "OptOutListArn")
            Prelude.<*> (x Core..?> "CreatedTimestamp")
            Prelude.<*> (x Core..?> "OptOutListName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateOptOutList where
  hashWithSalt _salt CreateOptOutList' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` optOutListName

instance Prelude.NFData CreateOptOutList where
  rnf CreateOptOutList' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf optOutListName

instance Core.ToHeaders CreateOptOutList where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "PinpointSMSVoiceV2.CreateOptOutList" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateOptOutList where
  toJSON CreateOptOutList' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            ("ClientToken" Core..=) Prelude.<$> clientToken,
            Prelude.Just
              ("OptOutListName" Core..= optOutListName)
          ]
      )

instance Core.ToPath CreateOptOutList where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateOptOutList where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateOptOutListResponse' smart constructor.
data CreateOptOutListResponse = CreateOptOutListResponse'
  { -- | An array of tags (key and value pairs) associated with the new
    -- OptOutList.
    tags :: Prelude.Maybe [Tag],
    -- | The Amazon Resource Name (ARN) for the OptOutList.
    optOutListArn :: Prelude.Maybe Prelude.Text,
    -- | The time when the pool was created, in
    -- <https://www.epochconverter.com/ UNIX epoch time> format.
    createdTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The name of the new OptOutList.
    optOutListName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateOptOutListResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createOptOutListResponse_tags' - An array of tags (key and value pairs) associated with the new
-- OptOutList.
--
-- 'optOutListArn', 'createOptOutListResponse_optOutListArn' - The Amazon Resource Name (ARN) for the OptOutList.
--
-- 'createdTimestamp', 'createOptOutListResponse_createdTimestamp' - The time when the pool was created, in
-- <https://www.epochconverter.com/ UNIX epoch time> format.
--
-- 'optOutListName', 'createOptOutListResponse_optOutListName' - The name of the new OptOutList.
--
-- 'httpStatus', 'createOptOutListResponse_httpStatus' - The response's http status code.
newCreateOptOutListResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateOptOutListResponse
newCreateOptOutListResponse pHttpStatus_ =
  CreateOptOutListResponse'
    { tags = Prelude.Nothing,
      optOutListArn = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing,
      optOutListName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of tags (key and value pairs) associated with the new
-- OptOutList.
createOptOutListResponse_tags :: Lens.Lens' CreateOptOutListResponse (Prelude.Maybe [Tag])
createOptOutListResponse_tags = Lens.lens (\CreateOptOutListResponse' {tags} -> tags) (\s@CreateOptOutListResponse' {} a -> s {tags = a} :: CreateOptOutListResponse) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) for the OptOutList.
createOptOutListResponse_optOutListArn :: Lens.Lens' CreateOptOutListResponse (Prelude.Maybe Prelude.Text)
createOptOutListResponse_optOutListArn = Lens.lens (\CreateOptOutListResponse' {optOutListArn} -> optOutListArn) (\s@CreateOptOutListResponse' {} a -> s {optOutListArn = a} :: CreateOptOutListResponse)

-- | The time when the pool was created, in
-- <https://www.epochconverter.com/ UNIX epoch time> format.
createOptOutListResponse_createdTimestamp :: Lens.Lens' CreateOptOutListResponse (Prelude.Maybe Prelude.UTCTime)
createOptOutListResponse_createdTimestamp = Lens.lens (\CreateOptOutListResponse' {createdTimestamp} -> createdTimestamp) (\s@CreateOptOutListResponse' {} a -> s {createdTimestamp = a} :: CreateOptOutListResponse) Prelude.. Lens.mapping Core._Time

-- | The name of the new OptOutList.
createOptOutListResponse_optOutListName :: Lens.Lens' CreateOptOutListResponse (Prelude.Maybe Prelude.Text)
createOptOutListResponse_optOutListName = Lens.lens (\CreateOptOutListResponse' {optOutListName} -> optOutListName) (\s@CreateOptOutListResponse' {} a -> s {optOutListName = a} :: CreateOptOutListResponse)

-- | The response's http status code.
createOptOutListResponse_httpStatus :: Lens.Lens' CreateOptOutListResponse Prelude.Int
createOptOutListResponse_httpStatus = Lens.lens (\CreateOptOutListResponse' {httpStatus} -> httpStatus) (\s@CreateOptOutListResponse' {} a -> s {httpStatus = a} :: CreateOptOutListResponse)

instance Prelude.NFData CreateOptOutListResponse where
  rnf CreateOptOutListResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf optOutListArn
      `Prelude.seq` Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf optOutListName
      `Prelude.seq` Prelude.rnf httpStatus
