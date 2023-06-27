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
-- Module      : Amazonka.FraudDetector.UpdateList
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a list.
module Amazonka.FraudDetector.UpdateList
  ( -- * Creating a Request
    UpdateList (..),
    newUpdateList,

    -- * Request Lenses
    updateList_description,
    updateList_elements,
    updateList_updateMode,
    updateList_variableType,
    updateList_name,

    -- * Destructuring the Response
    UpdateListResponse (..),
    newUpdateListResponse,

    -- * Response Lenses
    updateListResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateList' smart constructor.
data UpdateList = UpdateList'
  { -- | The new description.
    description :: Prelude.Maybe Prelude.Text,
    -- | One or more list elements to add or replace. If you are providing the
    -- elements, make sure to specify the @updateMode@ to use.
    --
    -- If you are deleting all elements from the list, use @REPLACE@ for the
    -- @updateMode@ and provide an empty list (0 elements).
    elements :: Prelude.Maybe [Data.Sensitive Prelude.Text],
    -- | The update mode (type).
    --
    -- -   Use @APPEND@ if you are adding elements to the list.
    --
    -- -   Use @REPLACE@ if you replacing existing elements in the list.
    --
    -- -   Use @REMOVE@ if you are removing elements from the list.
    updateMode :: Prelude.Maybe ListUpdateMode,
    -- | The variable type you want to assign to the list.
    --
    -- You cannot update a variable type of a list that already has a variable
    -- type assigned to it. You can assign a variable type to a list only if
    -- the list does not already have a variable type.
    variableType :: Prelude.Maybe Prelude.Text,
    -- | The name of the list to update.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateList_description' - The new description.
--
-- 'elements', 'updateList_elements' - One or more list elements to add or replace. If you are providing the
-- elements, make sure to specify the @updateMode@ to use.
--
-- If you are deleting all elements from the list, use @REPLACE@ for the
-- @updateMode@ and provide an empty list (0 elements).
--
-- 'updateMode', 'updateList_updateMode' - The update mode (type).
--
-- -   Use @APPEND@ if you are adding elements to the list.
--
-- -   Use @REPLACE@ if you replacing existing elements in the list.
--
-- -   Use @REMOVE@ if you are removing elements from the list.
--
-- 'variableType', 'updateList_variableType' - The variable type you want to assign to the list.
--
-- You cannot update a variable type of a list that already has a variable
-- type assigned to it. You can assign a variable type to a list only if
-- the list does not already have a variable type.
--
-- 'name', 'updateList_name' - The name of the list to update.
newUpdateList ::
  -- | 'name'
  Prelude.Text ->
  UpdateList
newUpdateList pName_ =
  UpdateList'
    { description = Prelude.Nothing,
      elements = Prelude.Nothing,
      updateMode = Prelude.Nothing,
      variableType = Prelude.Nothing,
      name = pName_
    }

-- | The new description.
updateList_description :: Lens.Lens' UpdateList (Prelude.Maybe Prelude.Text)
updateList_description = Lens.lens (\UpdateList' {description} -> description) (\s@UpdateList' {} a -> s {description = a} :: UpdateList)

-- | One or more list elements to add or replace. If you are providing the
-- elements, make sure to specify the @updateMode@ to use.
--
-- If you are deleting all elements from the list, use @REPLACE@ for the
-- @updateMode@ and provide an empty list (0 elements).
updateList_elements :: Lens.Lens' UpdateList (Prelude.Maybe [Prelude.Text])
updateList_elements = Lens.lens (\UpdateList' {elements} -> elements) (\s@UpdateList' {} a -> s {elements = a} :: UpdateList) Prelude.. Lens.mapping Lens.coerced

-- | The update mode (type).
--
-- -   Use @APPEND@ if you are adding elements to the list.
--
-- -   Use @REPLACE@ if you replacing existing elements in the list.
--
-- -   Use @REMOVE@ if you are removing elements from the list.
updateList_updateMode :: Lens.Lens' UpdateList (Prelude.Maybe ListUpdateMode)
updateList_updateMode = Lens.lens (\UpdateList' {updateMode} -> updateMode) (\s@UpdateList' {} a -> s {updateMode = a} :: UpdateList)

-- | The variable type you want to assign to the list.
--
-- You cannot update a variable type of a list that already has a variable
-- type assigned to it. You can assign a variable type to a list only if
-- the list does not already have a variable type.
updateList_variableType :: Lens.Lens' UpdateList (Prelude.Maybe Prelude.Text)
updateList_variableType = Lens.lens (\UpdateList' {variableType} -> variableType) (\s@UpdateList' {} a -> s {variableType = a} :: UpdateList)

-- | The name of the list to update.
updateList_name :: Lens.Lens' UpdateList Prelude.Text
updateList_name = Lens.lens (\UpdateList' {name} -> name) (\s@UpdateList' {} a -> s {name = a} :: UpdateList)

instance Core.AWSRequest UpdateList where
  type AWSResponse UpdateList = UpdateListResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateListResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateList where
  hashWithSalt _salt UpdateList' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` elements
      `Prelude.hashWithSalt` updateMode
      `Prelude.hashWithSalt` variableType
      `Prelude.hashWithSalt` name

instance Prelude.NFData UpdateList where
  rnf UpdateList' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf elements
      `Prelude.seq` Prelude.rnf updateMode
      `Prelude.seq` Prelude.rnf variableType
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders UpdateList where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSHawksNestServiceFacade.UpdateList" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateList where
  toJSON UpdateList' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("elements" Data..=) Prelude.<$> elements,
            ("updateMode" Data..=) Prelude.<$> updateMode,
            ("variableType" Data..=) Prelude.<$> variableType,
            Prelude.Just ("name" Data..= name)
          ]
      )

instance Data.ToPath UpdateList where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateList where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateListResponse' smart constructor.
data UpdateListResponse = UpdateListResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateListResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateListResponse_httpStatus' - The response's http status code.
newUpdateListResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateListResponse
newUpdateListResponse pHttpStatus_ =
  UpdateListResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateListResponse_httpStatus :: Lens.Lens' UpdateListResponse Prelude.Int
updateListResponse_httpStatus = Lens.lens (\UpdateListResponse' {httpStatus} -> httpStatus) (\s@UpdateListResponse' {} a -> s {httpStatus = a} :: UpdateListResponse)

instance Prelude.NFData UpdateListResponse where
  rnf UpdateListResponse' {..} = Prelude.rnf httpStatus
