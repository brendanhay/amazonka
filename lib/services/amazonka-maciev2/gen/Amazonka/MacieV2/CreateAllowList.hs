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
-- Module      : Amazonka.MacieV2.CreateAllowList
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates and defines the settings for an allow list.
module Amazonka.MacieV2.CreateAllowList
  ( -- * Creating a Request
    CreateAllowList (..),
    newCreateAllowList,

    -- * Request Lenses
    createAllowList_description,
    createAllowList_tags,
    createAllowList_criteria,
    createAllowList_clientToken,
    createAllowList_name,

    -- * Destructuring the Response
    CreateAllowListResponse (..),
    newCreateAllowListResponse,

    -- * Response Lenses
    createAllowListResponse_arn,
    createAllowListResponse_id,
    createAllowListResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateAllowList' smart constructor.
data CreateAllowList = CreateAllowList'
  { -- | A custom description of the allow list. The description can contain as
    -- many as 512 characters.
    description :: Prelude.Maybe Prelude.Text,
    -- | A map of key-value pairs that specifies the tags to associate with the
    -- allow list.
    --
    -- An allow list can have a maximum of 50 tags. Each tag consists of a tag
    -- key and an associated tag value. The maximum length of a tag key is 128
    -- characters. The maximum length of a tag value is 256 characters.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The criteria that specify the text or text pattern to ignore. The
    -- criteria can be the location and name of an S3 object that lists
    -- specific text to ignore (s3WordsList), or a regular expression (regex)
    -- that defines a text pattern to ignore.
    criteria :: AllowListCriteria,
    -- | A unique, case-sensitive token that you provide to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Text,
    -- | A custom name for the allow list. The name can contain as many as 128
    -- characters.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAllowList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createAllowList_description' - A custom description of the allow list. The description can contain as
-- many as 512 characters.
--
-- 'tags', 'createAllowList_tags' - A map of key-value pairs that specifies the tags to associate with the
-- allow list.
--
-- An allow list can have a maximum of 50 tags. Each tag consists of a tag
-- key and an associated tag value. The maximum length of a tag key is 128
-- characters. The maximum length of a tag value is 256 characters.
--
-- 'criteria', 'createAllowList_criteria' - The criteria that specify the text or text pattern to ignore. The
-- criteria can be the location and name of an S3 object that lists
-- specific text to ignore (s3WordsList), or a regular expression (regex)
-- that defines a text pattern to ignore.
--
-- 'clientToken', 'createAllowList_clientToken' - A unique, case-sensitive token that you provide to ensure the
-- idempotency of the request.
--
-- 'name', 'createAllowList_name' - A custom name for the allow list. The name can contain as many as 128
-- characters.
newCreateAllowList ::
  -- | 'criteria'
  AllowListCriteria ->
  -- | 'clientToken'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  CreateAllowList
newCreateAllowList pCriteria_ pClientToken_ pName_ =
  CreateAllowList'
    { description = Prelude.Nothing,
      tags = Prelude.Nothing,
      criteria = pCriteria_,
      clientToken = pClientToken_,
      name = pName_
    }

-- | A custom description of the allow list. The description can contain as
-- many as 512 characters.
createAllowList_description :: Lens.Lens' CreateAllowList (Prelude.Maybe Prelude.Text)
createAllowList_description = Lens.lens (\CreateAllowList' {description} -> description) (\s@CreateAllowList' {} a -> s {description = a} :: CreateAllowList)

-- | A map of key-value pairs that specifies the tags to associate with the
-- allow list.
--
-- An allow list can have a maximum of 50 tags. Each tag consists of a tag
-- key and an associated tag value. The maximum length of a tag key is 128
-- characters. The maximum length of a tag value is 256 characters.
createAllowList_tags :: Lens.Lens' CreateAllowList (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createAllowList_tags = Lens.lens (\CreateAllowList' {tags} -> tags) (\s@CreateAllowList' {} a -> s {tags = a} :: CreateAllowList) Prelude.. Lens.mapping Lens.coerced

-- | The criteria that specify the text or text pattern to ignore. The
-- criteria can be the location and name of an S3 object that lists
-- specific text to ignore (s3WordsList), or a regular expression (regex)
-- that defines a text pattern to ignore.
createAllowList_criteria :: Lens.Lens' CreateAllowList AllowListCriteria
createAllowList_criteria = Lens.lens (\CreateAllowList' {criteria} -> criteria) (\s@CreateAllowList' {} a -> s {criteria = a} :: CreateAllowList)

-- | A unique, case-sensitive token that you provide to ensure the
-- idempotency of the request.
createAllowList_clientToken :: Lens.Lens' CreateAllowList Prelude.Text
createAllowList_clientToken = Lens.lens (\CreateAllowList' {clientToken} -> clientToken) (\s@CreateAllowList' {} a -> s {clientToken = a} :: CreateAllowList)

-- | A custom name for the allow list. The name can contain as many as 128
-- characters.
createAllowList_name :: Lens.Lens' CreateAllowList Prelude.Text
createAllowList_name = Lens.lens (\CreateAllowList' {name} -> name) (\s@CreateAllowList' {} a -> s {name = a} :: CreateAllowList)

instance Core.AWSRequest CreateAllowList where
  type
    AWSResponse CreateAllowList =
      CreateAllowListResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAllowListResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateAllowList where
  hashWithSalt _salt CreateAllowList' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` criteria
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateAllowList where
  rnf CreateAllowList' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf tags `Prelude.seq`
        Prelude.rnf criteria `Prelude.seq`
          Prelude.rnf clientToken `Prelude.seq`
            Prelude.rnf name

instance Data.ToHeaders CreateAllowList where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateAllowList where
  toJSON CreateAllowList' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("criteria" Data..= criteria),
            Prelude.Just ("clientToken" Data..= clientToken),
            Prelude.Just ("name" Data..= name)
          ]
      )

instance Data.ToPath CreateAllowList where
  toPath = Prelude.const "/allow-lists"

instance Data.ToQuery CreateAllowList where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAllowListResponse' smart constructor.
data CreateAllowListResponse = CreateAllowListResponse'
  { -- | The Amazon Resource Name (ARN) of the allow list.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the allow list.
    id :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAllowListResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createAllowListResponse_arn' - The Amazon Resource Name (ARN) of the allow list.
--
-- 'id', 'createAllowListResponse_id' - The unique identifier for the allow list.
--
-- 'httpStatus', 'createAllowListResponse_httpStatus' - The response's http status code.
newCreateAllowListResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateAllowListResponse
newCreateAllowListResponse pHttpStatus_ =
  CreateAllowListResponse'
    { arn = Prelude.Nothing,
      id = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the allow list.
createAllowListResponse_arn :: Lens.Lens' CreateAllowListResponse (Prelude.Maybe Prelude.Text)
createAllowListResponse_arn = Lens.lens (\CreateAllowListResponse' {arn} -> arn) (\s@CreateAllowListResponse' {} a -> s {arn = a} :: CreateAllowListResponse)

-- | The unique identifier for the allow list.
createAllowListResponse_id :: Lens.Lens' CreateAllowListResponse (Prelude.Maybe Prelude.Text)
createAllowListResponse_id = Lens.lens (\CreateAllowListResponse' {id} -> id) (\s@CreateAllowListResponse' {} a -> s {id = a} :: CreateAllowListResponse)

-- | The response's http status code.
createAllowListResponse_httpStatus :: Lens.Lens' CreateAllowListResponse Prelude.Int
createAllowListResponse_httpStatus = Lens.lens (\CreateAllowListResponse' {httpStatus} -> httpStatus) (\s@CreateAllowListResponse' {} a -> s {httpStatus = a} :: CreateAllowListResponse)

instance Prelude.NFData CreateAllowListResponse where
  rnf CreateAllowListResponse' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf id `Prelude.seq`
        Prelude.rnf httpStatus
