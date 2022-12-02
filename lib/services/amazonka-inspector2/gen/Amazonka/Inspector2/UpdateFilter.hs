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
-- Module      : Amazonka.Inspector2.UpdateFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Specifies the action that is to be applied to the findings that match
-- the filter.
module Amazonka.Inspector2.UpdateFilter
  ( -- * Creating a Request
    UpdateFilter (..),
    newUpdateFilter,

    -- * Request Lenses
    updateFilter_name,
    updateFilter_description,
    updateFilter_filterCriteria,
    updateFilter_reason,
    updateFilter_action,
    updateFilter_filterArn,

    -- * Destructuring the Response
    UpdateFilterResponse (..),
    newUpdateFilterResponse,

    -- * Response Lenses
    updateFilterResponse_httpStatus,
    updateFilterResponse_arn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateFilter' smart constructor.
data UpdateFilter = UpdateFilter'
  { -- | The name of the filter.
    name :: Prelude.Maybe Prelude.Text,
    -- | A description of the filter.
    description :: Prelude.Maybe Prelude.Text,
    -- | Defines the criteria to be update in the filter.
    filterCriteria :: Prelude.Maybe FilterCriteria,
    -- | The reason the filter was updated.
    reason :: Prelude.Maybe Prelude.Text,
    -- | Specifies the action that is to be applied to the findings that match
    -- the filter.
    action :: Prelude.Maybe FilterAction,
    -- | The Amazon Resource Number (ARN) of the filter to update.
    filterArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateFilter_name' - The name of the filter.
--
-- 'description', 'updateFilter_description' - A description of the filter.
--
-- 'filterCriteria', 'updateFilter_filterCriteria' - Defines the criteria to be update in the filter.
--
-- 'reason', 'updateFilter_reason' - The reason the filter was updated.
--
-- 'action', 'updateFilter_action' - Specifies the action that is to be applied to the findings that match
-- the filter.
--
-- 'filterArn', 'updateFilter_filterArn' - The Amazon Resource Number (ARN) of the filter to update.
newUpdateFilter ::
  -- | 'filterArn'
  Prelude.Text ->
  UpdateFilter
newUpdateFilter pFilterArn_ =
  UpdateFilter'
    { name = Prelude.Nothing,
      description = Prelude.Nothing,
      filterCriteria = Prelude.Nothing,
      reason = Prelude.Nothing,
      action = Prelude.Nothing,
      filterArn = pFilterArn_
    }

-- | The name of the filter.
updateFilter_name :: Lens.Lens' UpdateFilter (Prelude.Maybe Prelude.Text)
updateFilter_name = Lens.lens (\UpdateFilter' {name} -> name) (\s@UpdateFilter' {} a -> s {name = a} :: UpdateFilter)

-- | A description of the filter.
updateFilter_description :: Lens.Lens' UpdateFilter (Prelude.Maybe Prelude.Text)
updateFilter_description = Lens.lens (\UpdateFilter' {description} -> description) (\s@UpdateFilter' {} a -> s {description = a} :: UpdateFilter)

-- | Defines the criteria to be update in the filter.
updateFilter_filterCriteria :: Lens.Lens' UpdateFilter (Prelude.Maybe FilterCriteria)
updateFilter_filterCriteria = Lens.lens (\UpdateFilter' {filterCriteria} -> filterCriteria) (\s@UpdateFilter' {} a -> s {filterCriteria = a} :: UpdateFilter)

-- | The reason the filter was updated.
updateFilter_reason :: Lens.Lens' UpdateFilter (Prelude.Maybe Prelude.Text)
updateFilter_reason = Lens.lens (\UpdateFilter' {reason} -> reason) (\s@UpdateFilter' {} a -> s {reason = a} :: UpdateFilter)

-- | Specifies the action that is to be applied to the findings that match
-- the filter.
updateFilter_action :: Lens.Lens' UpdateFilter (Prelude.Maybe FilterAction)
updateFilter_action = Lens.lens (\UpdateFilter' {action} -> action) (\s@UpdateFilter' {} a -> s {action = a} :: UpdateFilter)

-- | The Amazon Resource Number (ARN) of the filter to update.
updateFilter_filterArn :: Lens.Lens' UpdateFilter Prelude.Text
updateFilter_filterArn = Lens.lens (\UpdateFilter' {filterArn} -> filterArn) (\s@UpdateFilter' {} a -> s {filterArn = a} :: UpdateFilter)

instance Core.AWSRequest UpdateFilter where
  type AWSResponse UpdateFilter = UpdateFilterResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateFilterResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "arn")
      )

instance Prelude.Hashable UpdateFilter where
  hashWithSalt _salt UpdateFilter' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` filterCriteria
      `Prelude.hashWithSalt` reason
      `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` filterArn

instance Prelude.NFData UpdateFilter where
  rnf UpdateFilter' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf filterCriteria
      `Prelude.seq` Prelude.rnf reason
      `Prelude.seq` Prelude.rnf action
      `Prelude.seq` Prelude.rnf filterArn

instance Data.ToHeaders UpdateFilter where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateFilter where
  toJSON UpdateFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("name" Data..=) Prelude.<$> name,
            ("description" Data..=) Prelude.<$> description,
            ("filterCriteria" Data..=)
              Prelude.<$> filterCriteria,
            ("reason" Data..=) Prelude.<$> reason,
            ("action" Data..=) Prelude.<$> action,
            Prelude.Just ("filterArn" Data..= filterArn)
          ]
      )

instance Data.ToPath UpdateFilter where
  toPath = Prelude.const "/filters/update"

instance Data.ToQuery UpdateFilter where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateFilterResponse' smart constructor.
data UpdateFilterResponse = UpdateFilterResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Number (ARN) of the successfully updated filter.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFilterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateFilterResponse_httpStatus' - The response's http status code.
--
-- 'arn', 'updateFilterResponse_arn' - The Amazon Resource Number (ARN) of the successfully updated filter.
newUpdateFilterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'arn'
  Prelude.Text ->
  UpdateFilterResponse
newUpdateFilterResponse pHttpStatus_ pArn_ =
  UpdateFilterResponse'
    { httpStatus = pHttpStatus_,
      arn = pArn_
    }

-- | The response's http status code.
updateFilterResponse_httpStatus :: Lens.Lens' UpdateFilterResponse Prelude.Int
updateFilterResponse_httpStatus = Lens.lens (\UpdateFilterResponse' {httpStatus} -> httpStatus) (\s@UpdateFilterResponse' {} a -> s {httpStatus = a} :: UpdateFilterResponse)

-- | The Amazon Resource Number (ARN) of the successfully updated filter.
updateFilterResponse_arn :: Lens.Lens' UpdateFilterResponse Prelude.Text
updateFilterResponse_arn = Lens.lens (\UpdateFilterResponse' {arn} -> arn) (\s@UpdateFilterResponse' {} a -> s {arn = a} :: UpdateFilterResponse)

instance Prelude.NFData UpdateFilterResponse where
  rnf UpdateFilterResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf arn
