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
-- Module      : Amazonka.MacieV2.UpdateAllowList
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the settings for an allow list.
module Amazonka.MacieV2.UpdateAllowList
  ( -- * Creating a Request
    UpdateAllowList (..),
    newUpdateAllowList,

    -- * Request Lenses
    updateAllowList_description,
    updateAllowList_id,
    updateAllowList_criteria,
    updateAllowList_name,

    -- * Destructuring the Response
    UpdateAllowListResponse (..),
    newUpdateAllowListResponse,

    -- * Response Lenses
    updateAllowListResponse_arn,
    updateAllowListResponse_id,
    updateAllowListResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateAllowList' smart constructor.
data UpdateAllowList = UpdateAllowList'
  { -- | A custom description of the allow list. The description can contain as
    -- many as 512 characters.
    description :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the Amazon Macie resource that the request
    -- applies to.
    id :: Prelude.Text,
    -- | The criteria that specify the text or text pattern to ignore. The
    -- criteria can be the location and name of an S3 object that lists
    -- specific text to ignore (s3WordsList), or a regular expression that
    -- defines a text pattern to ignore (regex).
    --
    -- You can change a list\'s underlying criteria, such as the name of the S3
    -- object or the regular expression to use. However, you can\'t change the
    -- type from s3WordsList to regex or the other way around.
    criteria :: AllowListCriteria,
    -- | A custom name for the allow list. The name can contain as many as 128
    -- characters.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAllowList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateAllowList_description' - A custom description of the allow list. The description can contain as
-- many as 512 characters.
--
-- 'id', 'updateAllowList_id' - The unique identifier for the Amazon Macie resource that the request
-- applies to.
--
-- 'criteria', 'updateAllowList_criteria' - The criteria that specify the text or text pattern to ignore. The
-- criteria can be the location and name of an S3 object that lists
-- specific text to ignore (s3WordsList), or a regular expression that
-- defines a text pattern to ignore (regex).
--
-- You can change a list\'s underlying criteria, such as the name of the S3
-- object or the regular expression to use. However, you can\'t change the
-- type from s3WordsList to regex or the other way around.
--
-- 'name', 'updateAllowList_name' - A custom name for the allow list. The name can contain as many as 128
-- characters.
newUpdateAllowList ::
  -- | 'id'
  Prelude.Text ->
  -- | 'criteria'
  AllowListCriteria ->
  -- | 'name'
  Prelude.Text ->
  UpdateAllowList
newUpdateAllowList pId_ pCriteria_ pName_ =
  UpdateAllowList'
    { description = Prelude.Nothing,
      id = pId_,
      criteria = pCriteria_,
      name = pName_
    }

-- | A custom description of the allow list. The description can contain as
-- many as 512 characters.
updateAllowList_description :: Lens.Lens' UpdateAllowList (Prelude.Maybe Prelude.Text)
updateAllowList_description = Lens.lens (\UpdateAllowList' {description} -> description) (\s@UpdateAllowList' {} a -> s {description = a} :: UpdateAllowList)

-- | The unique identifier for the Amazon Macie resource that the request
-- applies to.
updateAllowList_id :: Lens.Lens' UpdateAllowList Prelude.Text
updateAllowList_id = Lens.lens (\UpdateAllowList' {id} -> id) (\s@UpdateAllowList' {} a -> s {id = a} :: UpdateAllowList)

-- | The criteria that specify the text or text pattern to ignore. The
-- criteria can be the location and name of an S3 object that lists
-- specific text to ignore (s3WordsList), or a regular expression that
-- defines a text pattern to ignore (regex).
--
-- You can change a list\'s underlying criteria, such as the name of the S3
-- object or the regular expression to use. However, you can\'t change the
-- type from s3WordsList to regex or the other way around.
updateAllowList_criteria :: Lens.Lens' UpdateAllowList AllowListCriteria
updateAllowList_criteria = Lens.lens (\UpdateAllowList' {criteria} -> criteria) (\s@UpdateAllowList' {} a -> s {criteria = a} :: UpdateAllowList)

-- | A custom name for the allow list. The name can contain as many as 128
-- characters.
updateAllowList_name :: Lens.Lens' UpdateAllowList Prelude.Text
updateAllowList_name = Lens.lens (\UpdateAllowList' {name} -> name) (\s@UpdateAllowList' {} a -> s {name = a} :: UpdateAllowList)

instance Core.AWSRequest UpdateAllowList where
  type
    AWSResponse UpdateAllowList =
      UpdateAllowListResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateAllowListResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateAllowList where
  hashWithSalt _salt UpdateAllowList' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` criteria
      `Prelude.hashWithSalt` name

instance Prelude.NFData UpdateAllowList where
  rnf UpdateAllowList' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf criteria
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders UpdateAllowList where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateAllowList where
  toJSON UpdateAllowList' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            Prelude.Just ("criteria" Data..= criteria),
            Prelude.Just ("name" Data..= name)
          ]
      )

instance Data.ToPath UpdateAllowList where
  toPath UpdateAllowList' {..} =
    Prelude.mconcat ["/allow-lists/", Data.toBS id]

instance Data.ToQuery UpdateAllowList where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAllowListResponse' smart constructor.
data UpdateAllowListResponse = UpdateAllowListResponse'
  { -- | The Amazon Resource Name (ARN) of the allow list.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the allow list.
    id :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAllowListResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'updateAllowListResponse_arn' - The Amazon Resource Name (ARN) of the allow list.
--
-- 'id', 'updateAllowListResponse_id' - The unique identifier for the allow list.
--
-- 'httpStatus', 'updateAllowListResponse_httpStatus' - The response's http status code.
newUpdateAllowListResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateAllowListResponse
newUpdateAllowListResponse pHttpStatus_ =
  UpdateAllowListResponse'
    { arn = Prelude.Nothing,
      id = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the allow list.
updateAllowListResponse_arn :: Lens.Lens' UpdateAllowListResponse (Prelude.Maybe Prelude.Text)
updateAllowListResponse_arn = Lens.lens (\UpdateAllowListResponse' {arn} -> arn) (\s@UpdateAllowListResponse' {} a -> s {arn = a} :: UpdateAllowListResponse)

-- | The unique identifier for the allow list.
updateAllowListResponse_id :: Lens.Lens' UpdateAllowListResponse (Prelude.Maybe Prelude.Text)
updateAllowListResponse_id = Lens.lens (\UpdateAllowListResponse' {id} -> id) (\s@UpdateAllowListResponse' {} a -> s {id = a} :: UpdateAllowListResponse)

-- | The response's http status code.
updateAllowListResponse_httpStatus :: Lens.Lens' UpdateAllowListResponse Prelude.Int
updateAllowListResponse_httpStatus = Lens.lens (\UpdateAllowListResponse' {httpStatus} -> httpStatus) (\s@UpdateAllowListResponse' {} a -> s {httpStatus = a} :: UpdateAllowListResponse)

instance Prelude.NFData UpdateAllowListResponse where
  rnf UpdateAllowListResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf httpStatus
