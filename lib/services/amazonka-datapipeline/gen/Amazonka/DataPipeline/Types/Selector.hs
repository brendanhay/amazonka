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
-- Module      : Amazonka.DataPipeline.Types.Selector
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataPipeline.Types.Selector where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DataPipeline.Types.Operator
import qualified Amazonka.Prelude as Prelude

-- | A comparision that is used to determine whether a query should return
-- this object.
--
-- /See:/ 'newSelector' smart constructor.
data Selector = Selector'
  { -- | The name of the field that the operator will be applied to. The field
    -- name is the \"key\" portion of the field definition in the pipeline
    -- definition syntax that is used by the AWS Data Pipeline API. If the
    -- field is not set on the object, the condition fails.
    fieldName :: Prelude.Maybe Prelude.Text,
    operator :: Prelude.Maybe Operator
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Selector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fieldName', 'selector_fieldName' - The name of the field that the operator will be applied to. The field
-- name is the \"key\" portion of the field definition in the pipeline
-- definition syntax that is used by the AWS Data Pipeline API. If the
-- field is not set on the object, the condition fails.
--
-- 'operator', 'selector_operator' - Undocumented member.
newSelector ::
  Selector
newSelector =
  Selector'
    { fieldName = Prelude.Nothing,
      operator = Prelude.Nothing
    }

-- | The name of the field that the operator will be applied to. The field
-- name is the \"key\" portion of the field definition in the pipeline
-- definition syntax that is used by the AWS Data Pipeline API. If the
-- field is not set on the object, the condition fails.
selector_fieldName :: Lens.Lens' Selector (Prelude.Maybe Prelude.Text)
selector_fieldName = Lens.lens (\Selector' {fieldName} -> fieldName) (\s@Selector' {} a -> s {fieldName = a} :: Selector)

-- | Undocumented member.
selector_operator :: Lens.Lens' Selector (Prelude.Maybe Operator)
selector_operator = Lens.lens (\Selector' {operator} -> operator) (\s@Selector' {} a -> s {operator = a} :: Selector)

instance Prelude.Hashable Selector where
  hashWithSalt _salt Selector' {..} =
    _salt `Prelude.hashWithSalt` fieldName
      `Prelude.hashWithSalt` operator

instance Prelude.NFData Selector where
  rnf Selector' {..} =
    Prelude.rnf fieldName
      `Prelude.seq` Prelude.rnf operator

instance Core.ToJSON Selector where
  toJSON Selector' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("fieldName" Core..=) Prelude.<$> fieldName,
            ("operator" Core..=) Prelude.<$> operator
          ]
      )
