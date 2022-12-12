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
-- Module      : Amazonka.Inspector2.CreateFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a filter resource using specified filter criteria.
module Amazonka.Inspector2.CreateFilter
  ( -- * Creating a Request
    CreateFilter (..),
    newCreateFilter,

    -- * Request Lenses
    createFilter_description,
    createFilter_reason,
    createFilter_tags,
    createFilter_action,
    createFilter_filterCriteria,
    createFilter_name,

    -- * Destructuring the Response
    CreateFilterResponse (..),
    newCreateFilterResponse,

    -- * Response Lenses
    createFilterResponse_httpStatus,
    createFilterResponse_arn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateFilter' smart constructor.
data CreateFilter = CreateFilter'
  { -- | A description of the filter.
    description :: Prelude.Maybe Prelude.Text,
    -- | The reason for creating the filter.
    reason :: Prelude.Maybe Prelude.Text,
    -- | A list of tags for the filter.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Defines the action that is to be applied to the findings that match the
    -- filter.
    action :: FilterAction,
    -- | Defines the criteria to be used in the filter for querying findings.
    filterCriteria :: FilterCriteria,
    -- | The name of the filter. Minimum length of 3. Maximum length of 64. Valid
    -- characters include alphanumeric characters, dot (.), underscore (_), and
    -- dash (-). Spaces are not allowed.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createFilter_description' - A description of the filter.
--
-- 'reason', 'createFilter_reason' - The reason for creating the filter.
--
-- 'tags', 'createFilter_tags' - A list of tags for the filter.
--
-- 'action', 'createFilter_action' - Defines the action that is to be applied to the findings that match the
-- filter.
--
-- 'filterCriteria', 'createFilter_filterCriteria' - Defines the criteria to be used in the filter for querying findings.
--
-- 'name', 'createFilter_name' - The name of the filter. Minimum length of 3. Maximum length of 64. Valid
-- characters include alphanumeric characters, dot (.), underscore (_), and
-- dash (-). Spaces are not allowed.
newCreateFilter ::
  -- | 'action'
  FilterAction ->
  -- | 'filterCriteria'
  FilterCriteria ->
  -- | 'name'
  Prelude.Text ->
  CreateFilter
newCreateFilter pAction_ pFilterCriteria_ pName_ =
  CreateFilter'
    { description = Prelude.Nothing,
      reason = Prelude.Nothing,
      tags = Prelude.Nothing,
      action = pAction_,
      filterCriteria = pFilterCriteria_,
      name = pName_
    }

-- | A description of the filter.
createFilter_description :: Lens.Lens' CreateFilter (Prelude.Maybe Prelude.Text)
createFilter_description = Lens.lens (\CreateFilter' {description} -> description) (\s@CreateFilter' {} a -> s {description = a} :: CreateFilter)

-- | The reason for creating the filter.
createFilter_reason :: Lens.Lens' CreateFilter (Prelude.Maybe Prelude.Text)
createFilter_reason = Lens.lens (\CreateFilter' {reason} -> reason) (\s@CreateFilter' {} a -> s {reason = a} :: CreateFilter)

-- | A list of tags for the filter.
createFilter_tags :: Lens.Lens' CreateFilter (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createFilter_tags = Lens.lens (\CreateFilter' {tags} -> tags) (\s@CreateFilter' {} a -> s {tags = a} :: CreateFilter) Prelude.. Lens.mapping Lens.coerced

-- | Defines the action that is to be applied to the findings that match the
-- filter.
createFilter_action :: Lens.Lens' CreateFilter FilterAction
createFilter_action = Lens.lens (\CreateFilter' {action} -> action) (\s@CreateFilter' {} a -> s {action = a} :: CreateFilter)

-- | Defines the criteria to be used in the filter for querying findings.
createFilter_filterCriteria :: Lens.Lens' CreateFilter FilterCriteria
createFilter_filterCriteria = Lens.lens (\CreateFilter' {filterCriteria} -> filterCriteria) (\s@CreateFilter' {} a -> s {filterCriteria = a} :: CreateFilter)

-- | The name of the filter. Minimum length of 3. Maximum length of 64. Valid
-- characters include alphanumeric characters, dot (.), underscore (_), and
-- dash (-). Spaces are not allowed.
createFilter_name :: Lens.Lens' CreateFilter Prelude.Text
createFilter_name = Lens.lens (\CreateFilter' {name} -> name) (\s@CreateFilter' {} a -> s {name = a} :: CreateFilter)

instance Core.AWSRequest CreateFilter where
  type AWSResponse CreateFilter = CreateFilterResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateFilterResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "arn")
      )

instance Prelude.Hashable CreateFilter where
  hashWithSalt _salt CreateFilter' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` reason
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` filterCriteria
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateFilter where
  rnf CreateFilter' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf reason
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf action
      `Prelude.seq` Prelude.rnf filterCriteria
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders CreateFilter where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateFilter where
  toJSON CreateFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("reason" Data..=) Prelude.<$> reason,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("action" Data..= action),
            Prelude.Just
              ("filterCriteria" Data..= filterCriteria),
            Prelude.Just ("name" Data..= name)
          ]
      )

instance Data.ToPath CreateFilter where
  toPath = Prelude.const "/filters/create"

instance Data.ToQuery CreateFilter where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateFilterResponse' smart constructor.
data CreateFilterResponse = CreateFilterResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Number (ARN) of the successfully created filter.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFilterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createFilterResponse_httpStatus' - The response's http status code.
--
-- 'arn', 'createFilterResponse_arn' - The Amazon Resource Number (ARN) of the successfully created filter.
newCreateFilterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'arn'
  Prelude.Text ->
  CreateFilterResponse
newCreateFilterResponse pHttpStatus_ pArn_ =
  CreateFilterResponse'
    { httpStatus = pHttpStatus_,
      arn = pArn_
    }

-- | The response's http status code.
createFilterResponse_httpStatus :: Lens.Lens' CreateFilterResponse Prelude.Int
createFilterResponse_httpStatus = Lens.lens (\CreateFilterResponse' {httpStatus} -> httpStatus) (\s@CreateFilterResponse' {} a -> s {httpStatus = a} :: CreateFilterResponse)

-- | The Amazon Resource Number (ARN) of the successfully created filter.
createFilterResponse_arn :: Lens.Lens' CreateFilterResponse Prelude.Text
createFilterResponse_arn = Lens.lens (\CreateFilterResponse' {arn} -> arn) (\s@CreateFilterResponse' {} a -> s {arn = a} :: CreateFilterResponse)

instance Prelude.NFData CreateFilterResponse where
  rnf CreateFilterResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf arn
