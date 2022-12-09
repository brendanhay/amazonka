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
    createOptOutList_clientToken,
    createOptOutList_tags,
    createOptOutList_optOutListName,

    -- * Destructuring the Response
    CreateOptOutListResponse (..),
    newCreateOptOutListResponse,

    -- * Response Lenses
    createOptOutListResponse_createdTimestamp,
    createOptOutListResponse_optOutListArn,
    createOptOutListResponse_optOutListName,
    createOptOutListResponse_tags,
    createOptOutListResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointSmsVoiceV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateOptOutList' smart constructor.
data CreateOptOutList = CreateOptOutList'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. If you don\'t specify a client token, a
    -- randomly generated token is used for the request to ensure idempotency.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | An array of tags (key and value pairs) to associate with the new
    -- OptOutList.
    tags :: Prelude.Maybe [Tag],
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
-- 'clientToken', 'createOptOutList_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you don\'t specify a client token, a
-- randomly generated token is used for the request to ensure idempotency.
--
-- 'tags', 'createOptOutList_tags' - An array of tags (key and value pairs) to associate with the new
-- OptOutList.
--
-- 'optOutListName', 'createOptOutList_optOutListName' - The name of the new OptOutList.
newCreateOptOutList ::
  -- | 'optOutListName'
  Prelude.Text ->
  CreateOptOutList
newCreateOptOutList pOptOutListName_ =
  CreateOptOutList'
    { clientToken = Prelude.Nothing,
      tags = Prelude.Nothing,
      optOutListName = pOptOutListName_
    }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you don\'t specify a client token, a
-- randomly generated token is used for the request to ensure idempotency.
createOptOutList_clientToken :: Lens.Lens' CreateOptOutList (Prelude.Maybe Prelude.Text)
createOptOutList_clientToken = Lens.lens (\CreateOptOutList' {clientToken} -> clientToken) (\s@CreateOptOutList' {} a -> s {clientToken = a} :: CreateOptOutList)

-- | An array of tags (key and value pairs) to associate with the new
-- OptOutList.
createOptOutList_tags :: Lens.Lens' CreateOptOutList (Prelude.Maybe [Tag])
createOptOutList_tags = Lens.lens (\CreateOptOutList' {tags} -> tags) (\s@CreateOptOutList' {} a -> s {tags = a} :: CreateOptOutList) Prelude.. Lens.mapping Lens.coerced

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
            Prelude.<$> (x Data..?> "CreatedTimestamp")
            Prelude.<*> (x Data..?> "OptOutListArn")
            Prelude.<*> (x Data..?> "OptOutListName")
            Prelude.<*> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateOptOutList where
  hashWithSalt _salt CreateOptOutList' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` optOutListName

instance Prelude.NFData CreateOptOutList where
  rnf CreateOptOutList' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf optOutListName

instance Data.ToHeaders CreateOptOutList where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PinpointSMSVoiceV2.CreateOptOutList" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateOptOutList where
  toJSON CreateOptOutList' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("OptOutListName" Data..= optOutListName)
          ]
      )

instance Data.ToPath CreateOptOutList where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateOptOutList where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateOptOutListResponse' smart constructor.
data CreateOptOutListResponse = CreateOptOutListResponse'
  { -- | The time when the pool was created, in
    -- <https://www.epochconverter.com/ UNIX epoch time> format.
    createdTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) for the OptOutList.
    optOutListArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the new OptOutList.
    optOutListName :: Prelude.Maybe Prelude.Text,
    -- | An array of tags (key and value pairs) associated with the new
    -- OptOutList.
    tags :: Prelude.Maybe [Tag],
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
-- 'createdTimestamp', 'createOptOutListResponse_createdTimestamp' - The time when the pool was created, in
-- <https://www.epochconverter.com/ UNIX epoch time> format.
--
-- 'optOutListArn', 'createOptOutListResponse_optOutListArn' - The Amazon Resource Name (ARN) for the OptOutList.
--
-- 'optOutListName', 'createOptOutListResponse_optOutListName' - The name of the new OptOutList.
--
-- 'tags', 'createOptOutListResponse_tags' - An array of tags (key and value pairs) associated with the new
-- OptOutList.
--
-- 'httpStatus', 'createOptOutListResponse_httpStatus' - The response's http status code.
newCreateOptOutListResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateOptOutListResponse
newCreateOptOutListResponse pHttpStatus_ =
  CreateOptOutListResponse'
    { createdTimestamp =
        Prelude.Nothing,
      optOutListArn = Prelude.Nothing,
      optOutListName = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time when the pool was created, in
-- <https://www.epochconverter.com/ UNIX epoch time> format.
createOptOutListResponse_createdTimestamp :: Lens.Lens' CreateOptOutListResponse (Prelude.Maybe Prelude.UTCTime)
createOptOutListResponse_createdTimestamp = Lens.lens (\CreateOptOutListResponse' {createdTimestamp} -> createdTimestamp) (\s@CreateOptOutListResponse' {} a -> s {createdTimestamp = a} :: CreateOptOutListResponse) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) for the OptOutList.
createOptOutListResponse_optOutListArn :: Lens.Lens' CreateOptOutListResponse (Prelude.Maybe Prelude.Text)
createOptOutListResponse_optOutListArn = Lens.lens (\CreateOptOutListResponse' {optOutListArn} -> optOutListArn) (\s@CreateOptOutListResponse' {} a -> s {optOutListArn = a} :: CreateOptOutListResponse)

-- | The name of the new OptOutList.
createOptOutListResponse_optOutListName :: Lens.Lens' CreateOptOutListResponse (Prelude.Maybe Prelude.Text)
createOptOutListResponse_optOutListName = Lens.lens (\CreateOptOutListResponse' {optOutListName} -> optOutListName) (\s@CreateOptOutListResponse' {} a -> s {optOutListName = a} :: CreateOptOutListResponse)

-- | An array of tags (key and value pairs) associated with the new
-- OptOutList.
createOptOutListResponse_tags :: Lens.Lens' CreateOptOutListResponse (Prelude.Maybe [Tag])
createOptOutListResponse_tags = Lens.lens (\CreateOptOutListResponse' {tags} -> tags) (\s@CreateOptOutListResponse' {} a -> s {tags = a} :: CreateOptOutListResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createOptOutListResponse_httpStatus :: Lens.Lens' CreateOptOutListResponse Prelude.Int
createOptOutListResponse_httpStatus = Lens.lens (\CreateOptOutListResponse' {httpStatus} -> httpStatus) (\s@CreateOptOutListResponse' {} a -> s {httpStatus = a} :: CreateOptOutListResponse)

instance Prelude.NFData CreateOptOutListResponse where
  rnf CreateOptOutListResponse' {..} =
    Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf optOutListArn
      `Prelude.seq` Prelude.rnf optOutListName
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
