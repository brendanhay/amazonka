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
-- Module      : Amazonka.Personalize.CreateFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a recommendation filter. For more information, see
-- <https://docs.aws.amazon.com/personalize/latest/dg/filter.html Filtering recommendations and user segments>.
module Amazonka.Personalize.CreateFilter
  ( -- * Creating a Request
    CreateFilter (..),
    newCreateFilter,

    -- * Request Lenses
    createFilter_tags,
    createFilter_name,
    createFilter_datasetGroupArn,
    createFilter_filterExpression,

    -- * Destructuring the Response
    CreateFilterResponse (..),
    newCreateFilterResponse,

    -- * Response Lenses
    createFilterResponse_filterArn,
    createFilterResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Personalize.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateFilter' smart constructor.
data CreateFilter = CreateFilter'
  { -- | A list of
    -- <https://docs.aws.amazon.com/personalize/latest/dev/tagging-resources.html tags>
    -- to apply to the filter.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the filter to create.
    name :: Prelude.Text,
    -- | The ARN of the dataset group that the filter will belong to.
    datasetGroupArn :: Prelude.Text,
    -- | The filter expression defines which items are included or excluded from
    -- recommendations. Filter expression must follow specific format rules.
    -- For information about filter expression structure and syntax, see
    -- <https://docs.aws.amazon.com/personalize/latest/dg/filter-expressions.html Filter expressions>.
    filterExpression :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createFilter_tags' - A list of
-- <https://docs.aws.amazon.com/personalize/latest/dev/tagging-resources.html tags>
-- to apply to the filter.
--
-- 'name', 'createFilter_name' - The name of the filter to create.
--
-- 'datasetGroupArn', 'createFilter_datasetGroupArn' - The ARN of the dataset group that the filter will belong to.
--
-- 'filterExpression', 'createFilter_filterExpression' - The filter expression defines which items are included or excluded from
-- recommendations. Filter expression must follow specific format rules.
-- For information about filter expression structure and syntax, see
-- <https://docs.aws.amazon.com/personalize/latest/dg/filter-expressions.html Filter expressions>.
newCreateFilter ::
  -- | 'name'
  Prelude.Text ->
  -- | 'datasetGroupArn'
  Prelude.Text ->
  -- | 'filterExpression'
  Prelude.Text ->
  CreateFilter
newCreateFilter
  pName_
  pDatasetGroupArn_
  pFilterExpression_ =
    CreateFilter'
      { tags = Prelude.Nothing,
        name = pName_,
        datasetGroupArn = pDatasetGroupArn_,
        filterExpression =
          Data._Sensitive Lens.# pFilterExpression_
      }

-- | A list of
-- <https://docs.aws.amazon.com/personalize/latest/dev/tagging-resources.html tags>
-- to apply to the filter.
createFilter_tags :: Lens.Lens' CreateFilter (Prelude.Maybe [Tag])
createFilter_tags = Lens.lens (\CreateFilter' {tags} -> tags) (\s@CreateFilter' {} a -> s {tags = a} :: CreateFilter) Prelude.. Lens.mapping Lens.coerced

-- | The name of the filter to create.
createFilter_name :: Lens.Lens' CreateFilter Prelude.Text
createFilter_name = Lens.lens (\CreateFilter' {name} -> name) (\s@CreateFilter' {} a -> s {name = a} :: CreateFilter)

-- | The ARN of the dataset group that the filter will belong to.
createFilter_datasetGroupArn :: Lens.Lens' CreateFilter Prelude.Text
createFilter_datasetGroupArn = Lens.lens (\CreateFilter' {datasetGroupArn} -> datasetGroupArn) (\s@CreateFilter' {} a -> s {datasetGroupArn = a} :: CreateFilter)

-- | The filter expression defines which items are included or excluded from
-- recommendations. Filter expression must follow specific format rules.
-- For information about filter expression structure and syntax, see
-- <https://docs.aws.amazon.com/personalize/latest/dg/filter-expressions.html Filter expressions>.
createFilter_filterExpression :: Lens.Lens' CreateFilter Prelude.Text
createFilter_filterExpression = Lens.lens (\CreateFilter' {filterExpression} -> filterExpression) (\s@CreateFilter' {} a -> s {filterExpression = a} :: CreateFilter) Prelude.. Data._Sensitive

instance Core.AWSRequest CreateFilter where
  type AWSResponse CreateFilter = CreateFilterResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateFilterResponse'
            Prelude.<$> (x Data..?> "filterArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateFilter where
  hashWithSalt _salt CreateFilter' {..} =
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` datasetGroupArn
      `Prelude.hashWithSalt` filterExpression

instance Prelude.NFData CreateFilter where
  rnf CreateFilter' {..} =
    Prelude.rnf tags `Prelude.seq`
      Prelude.rnf name `Prelude.seq`
        Prelude.rnf datasetGroupArn `Prelude.seq`
          Prelude.rnf filterExpression

instance Data.ToHeaders CreateFilter where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonPersonalize.CreateFilter" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateFilter where
  toJSON CreateFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("name" Data..= name),
            Prelude.Just
              ("datasetGroupArn" Data..= datasetGroupArn),
            Prelude.Just
              ("filterExpression" Data..= filterExpression)
          ]
      )

instance Data.ToPath CreateFilter where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateFilter where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateFilterResponse' smart constructor.
data CreateFilterResponse = CreateFilterResponse'
  { -- | The ARN of the new filter.
    filterArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
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
-- 'filterArn', 'createFilterResponse_filterArn' - The ARN of the new filter.
--
-- 'httpStatus', 'createFilterResponse_httpStatus' - The response's http status code.
newCreateFilterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateFilterResponse
newCreateFilterResponse pHttpStatus_ =
  CreateFilterResponse'
    { filterArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the new filter.
createFilterResponse_filterArn :: Lens.Lens' CreateFilterResponse (Prelude.Maybe Prelude.Text)
createFilterResponse_filterArn = Lens.lens (\CreateFilterResponse' {filterArn} -> filterArn) (\s@CreateFilterResponse' {} a -> s {filterArn = a} :: CreateFilterResponse)

-- | The response's http status code.
createFilterResponse_httpStatus :: Lens.Lens' CreateFilterResponse Prelude.Int
createFilterResponse_httpStatus = Lens.lens (\CreateFilterResponse' {httpStatus} -> httpStatus) (\s@CreateFilterResponse' {} a -> s {httpStatus = a} :: CreateFilterResponse)

instance Prelude.NFData CreateFilterResponse where
  rnf CreateFilterResponse' {..} =
    Prelude.rnf filterArn `Prelude.seq`
      Prelude.rnf httpStatus
