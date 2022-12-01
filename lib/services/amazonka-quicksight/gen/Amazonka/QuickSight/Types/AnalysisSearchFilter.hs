{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.QuickSight.Types.AnalysisSearchFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AnalysisSearchFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.AnalysisFilterAttribute
import Amazonka.QuickSight.Types.FilterOperator

-- | A filter that you apply when searching for one or more analyses.
--
-- /See:/ 'newAnalysisSearchFilter' smart constructor.
data AnalysisSearchFilter = AnalysisSearchFilter'
  { -- | The name of the value that you want to use as a filter, for example
    -- @\"Name\": \"QUICKSIGHT_OWNER\"@.
    --
    -- Valid values are defined as follows:
    --
    -- -   @QUICKSIGHT_VIEWER_OR_OWNER@: Provide an ARN of a user or group, and
    --     any analyses with that ARN listed as one of the analysis\' owners or
    --     viewers are returned. Implicit permissions from folders or groups
    --     are considered.
    --
    -- -   @QUICKSIGHT_OWNER@: Provide an ARN of a user or group, and any
    --     analyses with that ARN listed as one of the owners of the analyses
    --     are returned. Implicit permissions from folders or groups are
    --     considered.
    --
    -- -   @DIRECT_QUICKSIGHT_SOLE_OWNER@: Provide an ARN of a user or group,
    --     and any analyses with that ARN listed as the only owner of the
    --     analysis are returned. Implicit permissions from folders or groups
    --     are not considered.
    --
    -- -   @DIRECT_QUICKSIGHT_OWNER@: Provide an ARN of a user or group, and
    --     any analyses with that ARN listed as one of the owners of the
    --     analyses are returned. Implicit permissions from folders or groups
    --     are not considered.
    --
    -- -   @DIRECT_QUICKSIGHT_VIEWER_OR_OWNER@: Provide an ARN of a user or
    --     group, and any analyses with that ARN listed as one of the owners or
    --     viewers of the analyses are returned. Implicit permissions from
    --     folders or groups are not considered.
    --
    -- -   @ANALYSIS_NAME@: Any analyses whose names have a substring match to
    --     this value will be returned.
    name :: Prelude.Maybe AnalysisFilterAttribute,
    -- | The comparison operator that you want to use as a filter, for example
    -- @\"Operator\": \"StringEquals\"@. Valid values are @\"StringEquals\"@
    -- and @\"StringLike\"@.
    --
    -- If you set the operator value to @\"StringEquals\"@, you need to provide
    -- an ownership related filter in the @\"NAME\"@ field and the arn of the
    -- user or group whose folders you want to search in the @\"Value\"@ field.
    -- For example,
    -- @\"Name\":\"DIRECT_QUICKSIGHT_OWNER\", \"Operator\": \"StringEquals\", \"Value\": \"arn:aws:quicksight:us-east-1:1:user\/default\/UserName1\"@.
    --
    -- If you set the value to @\"StringLike\"@, you need to provide the name
    -- of the folders you are searching for. For example,
    -- @\"Name\":\"ANALYSIS_NAME\", \"Operator\": \"StringLike\", \"Value\": \"Test\"@.
    -- The @\"StringLike\"@ operator only supports the @NAME@ value
    -- @ANALYSIS_NAME@.
    operator :: Prelude.Maybe FilterOperator,
    -- | The value of the named item, in this case @QUICKSIGHT_USER@, that you
    -- want to use as a filter, for example @\"Value\"@. An example is
    -- @\"arn:aws:quicksight:us-east-1:1:user\/default\/UserName1\"@.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnalysisSearchFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'analysisSearchFilter_name' - The name of the value that you want to use as a filter, for example
-- @\"Name\": \"QUICKSIGHT_OWNER\"@.
--
-- Valid values are defined as follows:
--
-- -   @QUICKSIGHT_VIEWER_OR_OWNER@: Provide an ARN of a user or group, and
--     any analyses with that ARN listed as one of the analysis\' owners or
--     viewers are returned. Implicit permissions from folders or groups
--     are considered.
--
-- -   @QUICKSIGHT_OWNER@: Provide an ARN of a user or group, and any
--     analyses with that ARN listed as one of the owners of the analyses
--     are returned. Implicit permissions from folders or groups are
--     considered.
--
-- -   @DIRECT_QUICKSIGHT_SOLE_OWNER@: Provide an ARN of a user or group,
--     and any analyses with that ARN listed as the only owner of the
--     analysis are returned. Implicit permissions from folders or groups
--     are not considered.
--
-- -   @DIRECT_QUICKSIGHT_OWNER@: Provide an ARN of a user or group, and
--     any analyses with that ARN listed as one of the owners of the
--     analyses are returned. Implicit permissions from folders or groups
--     are not considered.
--
-- -   @DIRECT_QUICKSIGHT_VIEWER_OR_OWNER@: Provide an ARN of a user or
--     group, and any analyses with that ARN listed as one of the owners or
--     viewers of the analyses are returned. Implicit permissions from
--     folders or groups are not considered.
--
-- -   @ANALYSIS_NAME@: Any analyses whose names have a substring match to
--     this value will be returned.
--
-- 'operator', 'analysisSearchFilter_operator' - The comparison operator that you want to use as a filter, for example
-- @\"Operator\": \"StringEquals\"@. Valid values are @\"StringEquals\"@
-- and @\"StringLike\"@.
--
-- If you set the operator value to @\"StringEquals\"@, you need to provide
-- an ownership related filter in the @\"NAME\"@ field and the arn of the
-- user or group whose folders you want to search in the @\"Value\"@ field.
-- For example,
-- @\"Name\":\"DIRECT_QUICKSIGHT_OWNER\", \"Operator\": \"StringEquals\", \"Value\": \"arn:aws:quicksight:us-east-1:1:user\/default\/UserName1\"@.
--
-- If you set the value to @\"StringLike\"@, you need to provide the name
-- of the folders you are searching for. For example,
-- @\"Name\":\"ANALYSIS_NAME\", \"Operator\": \"StringLike\", \"Value\": \"Test\"@.
-- The @\"StringLike\"@ operator only supports the @NAME@ value
-- @ANALYSIS_NAME@.
--
-- 'value', 'analysisSearchFilter_value' - The value of the named item, in this case @QUICKSIGHT_USER@, that you
-- want to use as a filter, for example @\"Value\"@. An example is
-- @\"arn:aws:quicksight:us-east-1:1:user\/default\/UserName1\"@.
newAnalysisSearchFilter ::
  AnalysisSearchFilter
newAnalysisSearchFilter =
  AnalysisSearchFilter'
    { name = Prelude.Nothing,
      operator = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The name of the value that you want to use as a filter, for example
-- @\"Name\": \"QUICKSIGHT_OWNER\"@.
--
-- Valid values are defined as follows:
--
-- -   @QUICKSIGHT_VIEWER_OR_OWNER@: Provide an ARN of a user or group, and
--     any analyses with that ARN listed as one of the analysis\' owners or
--     viewers are returned. Implicit permissions from folders or groups
--     are considered.
--
-- -   @QUICKSIGHT_OWNER@: Provide an ARN of a user or group, and any
--     analyses with that ARN listed as one of the owners of the analyses
--     are returned. Implicit permissions from folders or groups are
--     considered.
--
-- -   @DIRECT_QUICKSIGHT_SOLE_OWNER@: Provide an ARN of a user or group,
--     and any analyses with that ARN listed as the only owner of the
--     analysis are returned. Implicit permissions from folders or groups
--     are not considered.
--
-- -   @DIRECT_QUICKSIGHT_OWNER@: Provide an ARN of a user or group, and
--     any analyses with that ARN listed as one of the owners of the
--     analyses are returned. Implicit permissions from folders or groups
--     are not considered.
--
-- -   @DIRECT_QUICKSIGHT_VIEWER_OR_OWNER@: Provide an ARN of a user or
--     group, and any analyses with that ARN listed as one of the owners or
--     viewers of the analyses are returned. Implicit permissions from
--     folders or groups are not considered.
--
-- -   @ANALYSIS_NAME@: Any analyses whose names have a substring match to
--     this value will be returned.
analysisSearchFilter_name :: Lens.Lens' AnalysisSearchFilter (Prelude.Maybe AnalysisFilterAttribute)
analysisSearchFilter_name = Lens.lens (\AnalysisSearchFilter' {name} -> name) (\s@AnalysisSearchFilter' {} a -> s {name = a} :: AnalysisSearchFilter)

-- | The comparison operator that you want to use as a filter, for example
-- @\"Operator\": \"StringEquals\"@. Valid values are @\"StringEquals\"@
-- and @\"StringLike\"@.
--
-- If you set the operator value to @\"StringEquals\"@, you need to provide
-- an ownership related filter in the @\"NAME\"@ field and the arn of the
-- user or group whose folders you want to search in the @\"Value\"@ field.
-- For example,
-- @\"Name\":\"DIRECT_QUICKSIGHT_OWNER\", \"Operator\": \"StringEquals\", \"Value\": \"arn:aws:quicksight:us-east-1:1:user\/default\/UserName1\"@.
--
-- If you set the value to @\"StringLike\"@, you need to provide the name
-- of the folders you are searching for. For example,
-- @\"Name\":\"ANALYSIS_NAME\", \"Operator\": \"StringLike\", \"Value\": \"Test\"@.
-- The @\"StringLike\"@ operator only supports the @NAME@ value
-- @ANALYSIS_NAME@.
analysisSearchFilter_operator :: Lens.Lens' AnalysisSearchFilter (Prelude.Maybe FilterOperator)
analysisSearchFilter_operator = Lens.lens (\AnalysisSearchFilter' {operator} -> operator) (\s@AnalysisSearchFilter' {} a -> s {operator = a} :: AnalysisSearchFilter)

-- | The value of the named item, in this case @QUICKSIGHT_USER@, that you
-- want to use as a filter, for example @\"Value\"@. An example is
-- @\"arn:aws:quicksight:us-east-1:1:user\/default\/UserName1\"@.
analysisSearchFilter_value :: Lens.Lens' AnalysisSearchFilter (Prelude.Maybe Prelude.Text)
analysisSearchFilter_value = Lens.lens (\AnalysisSearchFilter' {value} -> value) (\s@AnalysisSearchFilter' {} a -> s {value = a} :: AnalysisSearchFilter)

instance Prelude.Hashable AnalysisSearchFilter where
  hashWithSalt _salt AnalysisSearchFilter' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` operator
      `Prelude.hashWithSalt` value

instance Prelude.NFData AnalysisSearchFilter where
  rnf AnalysisSearchFilter' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf operator
      `Prelude.seq` Prelude.rnf value

instance Core.ToJSON AnalysisSearchFilter where
  toJSON AnalysisSearchFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Name" Core..=) Prelude.<$> name,
            ("Operator" Core..=) Prelude.<$> operator,
            ("Value" Core..=) Prelude.<$> value
          ]
      )
