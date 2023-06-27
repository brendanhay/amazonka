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
-- Module      : Amazonka.FraudDetector.CreateList
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a list.
--
-- List is a set of input data for a variable in your event dataset. You
-- use the input data in a rule that\'s associated with your detector. For
-- more information, see
-- <https://docs.aws.amazon.com/frauddetector/latest/ug/lists.html Lists>.
module Amazonka.FraudDetector.CreateList
  ( -- * Creating a Request
    CreateList (..),
    newCreateList,

    -- * Request Lenses
    createList_description,
    createList_elements,
    createList_tags,
    createList_variableType,
    createList_name,

    -- * Destructuring the Response
    CreateListResponse (..),
    newCreateListResponse,

    -- * Response Lenses
    createListResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateList' smart constructor.
data CreateList = CreateList'
  { -- | The description of the list.
    description :: Prelude.Maybe Prelude.Text,
    -- | The names of the elements, if providing. You can also create an empty
    -- list and add elements later using the
    -- <https://docs.aws.amazon.com/frauddetector/latest/api/API_Updatelist.html UpdateList>
    -- API.
    elements :: Prelude.Maybe [Data.Sensitive Prelude.Text],
    -- | A collection of the key and value pairs.
    tags :: Prelude.Maybe [Tag],
    -- | The variable type of the list. You can only assign the variable type
    -- with String data type. For more information, see
    -- <https://docs.aws.amazon.com/frauddetector/latest/ug/create-a-variable.html#variable-types Variable types>.
    variableType :: Prelude.Maybe Prelude.Text,
    -- | The name of the list.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createList_description' - The description of the list.
--
-- 'elements', 'createList_elements' - The names of the elements, if providing. You can also create an empty
-- list and add elements later using the
-- <https://docs.aws.amazon.com/frauddetector/latest/api/API_Updatelist.html UpdateList>
-- API.
--
-- 'tags', 'createList_tags' - A collection of the key and value pairs.
--
-- 'variableType', 'createList_variableType' - The variable type of the list. You can only assign the variable type
-- with String data type. For more information, see
-- <https://docs.aws.amazon.com/frauddetector/latest/ug/create-a-variable.html#variable-types Variable types>.
--
-- 'name', 'createList_name' - The name of the list.
newCreateList ::
  -- | 'name'
  Prelude.Text ->
  CreateList
newCreateList pName_ =
  CreateList'
    { description = Prelude.Nothing,
      elements = Prelude.Nothing,
      tags = Prelude.Nothing,
      variableType = Prelude.Nothing,
      name = pName_
    }

-- | The description of the list.
createList_description :: Lens.Lens' CreateList (Prelude.Maybe Prelude.Text)
createList_description = Lens.lens (\CreateList' {description} -> description) (\s@CreateList' {} a -> s {description = a} :: CreateList)

-- | The names of the elements, if providing. You can also create an empty
-- list and add elements later using the
-- <https://docs.aws.amazon.com/frauddetector/latest/api/API_Updatelist.html UpdateList>
-- API.
createList_elements :: Lens.Lens' CreateList (Prelude.Maybe [Prelude.Text])
createList_elements = Lens.lens (\CreateList' {elements} -> elements) (\s@CreateList' {} a -> s {elements = a} :: CreateList) Prelude.. Lens.mapping Lens.coerced

-- | A collection of the key and value pairs.
createList_tags :: Lens.Lens' CreateList (Prelude.Maybe [Tag])
createList_tags = Lens.lens (\CreateList' {tags} -> tags) (\s@CreateList' {} a -> s {tags = a} :: CreateList) Prelude.. Lens.mapping Lens.coerced

-- | The variable type of the list. You can only assign the variable type
-- with String data type. For more information, see
-- <https://docs.aws.amazon.com/frauddetector/latest/ug/create-a-variable.html#variable-types Variable types>.
createList_variableType :: Lens.Lens' CreateList (Prelude.Maybe Prelude.Text)
createList_variableType = Lens.lens (\CreateList' {variableType} -> variableType) (\s@CreateList' {} a -> s {variableType = a} :: CreateList)

-- | The name of the list.
createList_name :: Lens.Lens' CreateList Prelude.Text
createList_name = Lens.lens (\CreateList' {name} -> name) (\s@CreateList' {} a -> s {name = a} :: CreateList)

instance Core.AWSRequest CreateList where
  type AWSResponse CreateList = CreateListResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateListResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateList where
  hashWithSalt _salt CreateList' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` elements
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` variableType
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateList where
  rnf CreateList' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf elements
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf variableType
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders CreateList where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSHawksNestServiceFacade.CreateList" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateList where
  toJSON CreateList' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("elements" Data..=) Prelude.<$> elements,
            ("tags" Data..=) Prelude.<$> tags,
            ("variableType" Data..=) Prelude.<$> variableType,
            Prelude.Just ("name" Data..= name)
          ]
      )

instance Data.ToPath CreateList where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateList where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateListResponse' smart constructor.
data CreateListResponse = CreateListResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateListResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createListResponse_httpStatus' - The response's http status code.
newCreateListResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateListResponse
newCreateListResponse pHttpStatus_ =
  CreateListResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
createListResponse_httpStatus :: Lens.Lens' CreateListResponse Prelude.Int
createListResponse_httpStatus = Lens.lens (\CreateListResponse' {httpStatus} -> httpStatus) (\s@CreateListResponse' {} a -> s {httpStatus = a} :: CreateListResponse)

instance Prelude.NFData CreateListResponse where
  rnf CreateListResponse' {..} = Prelude.rnf httpStatus
