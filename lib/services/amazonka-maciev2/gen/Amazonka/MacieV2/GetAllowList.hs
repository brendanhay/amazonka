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
-- Module      : Amazonka.MacieV2.GetAllowList
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the settings and status of an allow list.
module Amazonka.MacieV2.GetAllowList
  ( -- * Creating a Request
    GetAllowList (..),
    newGetAllowList,

    -- * Request Lenses
    getAllowList_id,

    -- * Destructuring the Response
    GetAllowListResponse (..),
    newGetAllowListResponse,

    -- * Response Lenses
    getAllowListResponse_arn,
    getAllowListResponse_createdAt,
    getAllowListResponse_criteria,
    getAllowListResponse_description,
    getAllowListResponse_id,
    getAllowListResponse_name,
    getAllowListResponse_status,
    getAllowListResponse_tags,
    getAllowListResponse_updatedAt,
    getAllowListResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetAllowList' smart constructor.
data GetAllowList = GetAllowList'
  { -- | The unique identifier for the Amazon Macie resource that the request
    -- applies to.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAllowList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getAllowList_id' - The unique identifier for the Amazon Macie resource that the request
-- applies to.
newGetAllowList ::
  -- | 'id'
  Prelude.Text ->
  GetAllowList
newGetAllowList pId_ = GetAllowList' {id = pId_}

-- | The unique identifier for the Amazon Macie resource that the request
-- applies to.
getAllowList_id :: Lens.Lens' GetAllowList Prelude.Text
getAllowList_id = Lens.lens (\GetAllowList' {id} -> id) (\s@GetAllowList' {} a -> s {id = a} :: GetAllowList)

instance Core.AWSRequest GetAllowList where
  type AWSResponse GetAllowList = GetAllowListResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAllowListResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "createdAt")
            Prelude.<*> (x Data..?> "criteria")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "updatedAt")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAllowList where
  hashWithSalt _salt GetAllowList' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData GetAllowList where
  rnf GetAllowList' {..} = Prelude.rnf id

instance Data.ToHeaders GetAllowList where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetAllowList where
  toPath GetAllowList' {..} =
    Prelude.mconcat ["/allow-lists/", Data.toBS id]

instance Data.ToQuery GetAllowList where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAllowListResponse' smart constructor.
data GetAllowListResponse = GetAllowListResponse'
  { -- | The Amazon Resource Name (ARN) of the allow list.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and time, in UTC and extended ISO 8601 format, when the allow
    -- list was created in Amazon Macie.
    createdAt :: Prelude.Maybe Data.ISO8601,
    -- | The criteria that specify the text or text pattern to ignore. The
    -- criteria can be the location and name of an S3 object that lists
    -- specific text to ignore (s3WordsList), or a regular expression (regex)
    -- that defines a text pattern to ignore.
    criteria :: Prelude.Maybe AllowListCriteria,
    -- | The custom description of the allow list.
    description :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the allow list.
    id :: Prelude.Maybe Prelude.Text,
    -- | The custom name of the allow list.
    name :: Prelude.Maybe Prelude.Text,
    -- | The current status of the allow list, which indicates whether Amazon
    -- Macie can access and use the list\'s criteria.
    status :: Prelude.Maybe AllowListStatus,
    -- | A map of key-value pairs that specifies which tags (keys and values) are
    -- associated with the allow list.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The date and time, in UTC and extended ISO 8601 format, when the allow
    -- list\'s settings were most recently changed in Amazon Macie.
    updatedAt :: Prelude.Maybe Data.ISO8601,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAllowListResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getAllowListResponse_arn' - The Amazon Resource Name (ARN) of the allow list.
--
-- 'createdAt', 'getAllowListResponse_createdAt' - The date and time, in UTC and extended ISO 8601 format, when the allow
-- list was created in Amazon Macie.
--
-- 'criteria', 'getAllowListResponse_criteria' - The criteria that specify the text or text pattern to ignore. The
-- criteria can be the location and name of an S3 object that lists
-- specific text to ignore (s3WordsList), or a regular expression (regex)
-- that defines a text pattern to ignore.
--
-- 'description', 'getAllowListResponse_description' - The custom description of the allow list.
--
-- 'id', 'getAllowListResponse_id' - The unique identifier for the allow list.
--
-- 'name', 'getAllowListResponse_name' - The custom name of the allow list.
--
-- 'status', 'getAllowListResponse_status' - The current status of the allow list, which indicates whether Amazon
-- Macie can access and use the list\'s criteria.
--
-- 'tags', 'getAllowListResponse_tags' - A map of key-value pairs that specifies which tags (keys and values) are
-- associated with the allow list.
--
-- 'updatedAt', 'getAllowListResponse_updatedAt' - The date and time, in UTC and extended ISO 8601 format, when the allow
-- list\'s settings were most recently changed in Amazon Macie.
--
-- 'httpStatus', 'getAllowListResponse_httpStatus' - The response's http status code.
newGetAllowListResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAllowListResponse
newGetAllowListResponse pHttpStatus_ =
  GetAllowListResponse'
    { arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      criteria = Prelude.Nothing,
      description = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing,
      tags = Prelude.Nothing,
      updatedAt = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the allow list.
getAllowListResponse_arn :: Lens.Lens' GetAllowListResponse (Prelude.Maybe Prelude.Text)
getAllowListResponse_arn = Lens.lens (\GetAllowListResponse' {arn} -> arn) (\s@GetAllowListResponse' {} a -> s {arn = a} :: GetAllowListResponse)

-- | The date and time, in UTC and extended ISO 8601 format, when the allow
-- list was created in Amazon Macie.
getAllowListResponse_createdAt :: Lens.Lens' GetAllowListResponse (Prelude.Maybe Prelude.UTCTime)
getAllowListResponse_createdAt = Lens.lens (\GetAllowListResponse' {createdAt} -> createdAt) (\s@GetAllowListResponse' {} a -> s {createdAt = a} :: GetAllowListResponse) Prelude.. Lens.mapping Data._Time

-- | The criteria that specify the text or text pattern to ignore. The
-- criteria can be the location and name of an S3 object that lists
-- specific text to ignore (s3WordsList), or a regular expression (regex)
-- that defines a text pattern to ignore.
getAllowListResponse_criteria :: Lens.Lens' GetAllowListResponse (Prelude.Maybe AllowListCriteria)
getAllowListResponse_criteria = Lens.lens (\GetAllowListResponse' {criteria} -> criteria) (\s@GetAllowListResponse' {} a -> s {criteria = a} :: GetAllowListResponse)

-- | The custom description of the allow list.
getAllowListResponse_description :: Lens.Lens' GetAllowListResponse (Prelude.Maybe Prelude.Text)
getAllowListResponse_description = Lens.lens (\GetAllowListResponse' {description} -> description) (\s@GetAllowListResponse' {} a -> s {description = a} :: GetAllowListResponse)

-- | The unique identifier for the allow list.
getAllowListResponse_id :: Lens.Lens' GetAllowListResponse (Prelude.Maybe Prelude.Text)
getAllowListResponse_id = Lens.lens (\GetAllowListResponse' {id} -> id) (\s@GetAllowListResponse' {} a -> s {id = a} :: GetAllowListResponse)

-- | The custom name of the allow list.
getAllowListResponse_name :: Lens.Lens' GetAllowListResponse (Prelude.Maybe Prelude.Text)
getAllowListResponse_name = Lens.lens (\GetAllowListResponse' {name} -> name) (\s@GetAllowListResponse' {} a -> s {name = a} :: GetAllowListResponse)

-- | The current status of the allow list, which indicates whether Amazon
-- Macie can access and use the list\'s criteria.
getAllowListResponse_status :: Lens.Lens' GetAllowListResponse (Prelude.Maybe AllowListStatus)
getAllowListResponse_status = Lens.lens (\GetAllowListResponse' {status} -> status) (\s@GetAllowListResponse' {} a -> s {status = a} :: GetAllowListResponse)

-- | A map of key-value pairs that specifies which tags (keys and values) are
-- associated with the allow list.
getAllowListResponse_tags :: Lens.Lens' GetAllowListResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getAllowListResponse_tags = Lens.lens (\GetAllowListResponse' {tags} -> tags) (\s@GetAllowListResponse' {} a -> s {tags = a} :: GetAllowListResponse) Prelude.. Lens.mapping Lens.coerced

-- | The date and time, in UTC and extended ISO 8601 format, when the allow
-- list\'s settings were most recently changed in Amazon Macie.
getAllowListResponse_updatedAt :: Lens.Lens' GetAllowListResponse (Prelude.Maybe Prelude.UTCTime)
getAllowListResponse_updatedAt = Lens.lens (\GetAllowListResponse' {updatedAt} -> updatedAt) (\s@GetAllowListResponse' {} a -> s {updatedAt = a} :: GetAllowListResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
getAllowListResponse_httpStatus :: Lens.Lens' GetAllowListResponse Prelude.Int
getAllowListResponse_httpStatus = Lens.lens (\GetAllowListResponse' {httpStatus} -> httpStatus) (\s@GetAllowListResponse' {} a -> s {httpStatus = a} :: GetAllowListResponse)

instance Prelude.NFData GetAllowListResponse where
  rnf GetAllowListResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf criteria
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf updatedAt
      `Prelude.seq` Prelude.rnf httpStatus
