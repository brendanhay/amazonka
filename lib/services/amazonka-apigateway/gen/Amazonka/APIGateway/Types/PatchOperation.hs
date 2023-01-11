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
-- Module      : Amazonka.APIGateway.Types.PatchOperation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.APIGateway.Types.PatchOperation where

import Amazonka.APIGateway.Types.Op
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | For more information about supported patch operations, see
-- <https://docs.aws.amazon.com/apigateway/latest/api/patch-operations.html Patch Operations>.
--
-- /See:/ 'newPatchOperation' smart constructor.
data PatchOperation = PatchOperation'
  { -- | The copy update operation\'s source as identified by a JSON-Pointer
    -- value referencing the location within the targeted resource to copy the
    -- value from. For example, to promote a canary deployment, you copy the
    -- canary deployment ID to the affiliated deployment ID by calling a PATCH
    -- request on a Stage resource with \"op\":\"copy\",
    -- \"from\":\"\/canarySettings\/deploymentId\" and
    -- \"path\":\"\/deploymentId\".
    from :: Prelude.Maybe Prelude.Text,
    -- | An update operation to be performed with this PATCH request. The valid
    -- value can be add, remove, replace or copy. Not all valid operations are
    -- supported for a given resource. Support of the operations depends on
    -- specific operational contexts. Attempts to apply an unsupported
    -- operation on a resource will return an error message..
    op :: Prelude.Maybe Op,
    -- | The op operation\'s target, as identified by a JSON Pointer value that
    -- references a location within the targeted resource. For example, if the
    -- target resource has an updateable property of {\"name\":\"value\"}, the
    -- path for this property is \/name. If the name property value is a JSON
    -- object (e.g., {\"name\": {\"child\/name\": \"child-value\"}}), the path
    -- for the child\/name property will be \/name\/child~1name. Any slash
    -- (\"\/\") character appearing in path names must be escaped with \"~1\",
    -- as shown in the example above. Each op operation can have only one path
    -- associated with it.
    path :: Prelude.Maybe Prelude.Text,
    -- | The new target value of the update operation. It is applicable for the
    -- add or replace operation. When using AWS CLI to update a property of a
    -- JSON value, enclose the JSON object with a pair of single quotes in a
    -- Linux shell, e.g., \'{\"a\": ...}\'.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PatchOperation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'from', 'patchOperation_from' - The copy update operation\'s source as identified by a JSON-Pointer
-- value referencing the location within the targeted resource to copy the
-- value from. For example, to promote a canary deployment, you copy the
-- canary deployment ID to the affiliated deployment ID by calling a PATCH
-- request on a Stage resource with \"op\":\"copy\",
-- \"from\":\"\/canarySettings\/deploymentId\" and
-- \"path\":\"\/deploymentId\".
--
-- 'op', 'patchOperation_op' - An update operation to be performed with this PATCH request. The valid
-- value can be add, remove, replace or copy. Not all valid operations are
-- supported for a given resource. Support of the operations depends on
-- specific operational contexts. Attempts to apply an unsupported
-- operation on a resource will return an error message..
--
-- 'path', 'patchOperation_path' - The op operation\'s target, as identified by a JSON Pointer value that
-- references a location within the targeted resource. For example, if the
-- target resource has an updateable property of {\"name\":\"value\"}, the
-- path for this property is \/name. If the name property value is a JSON
-- object (e.g., {\"name\": {\"child\/name\": \"child-value\"}}), the path
-- for the child\/name property will be \/name\/child~1name. Any slash
-- (\"\/\") character appearing in path names must be escaped with \"~1\",
-- as shown in the example above. Each op operation can have only one path
-- associated with it.
--
-- 'value', 'patchOperation_value' - The new target value of the update operation. It is applicable for the
-- add or replace operation. When using AWS CLI to update a property of a
-- JSON value, enclose the JSON object with a pair of single quotes in a
-- Linux shell, e.g., \'{\"a\": ...}\'.
newPatchOperation ::
  PatchOperation
newPatchOperation =
  PatchOperation'
    { from = Prelude.Nothing,
      op = Prelude.Nothing,
      path = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The copy update operation\'s source as identified by a JSON-Pointer
-- value referencing the location within the targeted resource to copy the
-- value from. For example, to promote a canary deployment, you copy the
-- canary deployment ID to the affiliated deployment ID by calling a PATCH
-- request on a Stage resource with \"op\":\"copy\",
-- \"from\":\"\/canarySettings\/deploymentId\" and
-- \"path\":\"\/deploymentId\".
patchOperation_from :: Lens.Lens' PatchOperation (Prelude.Maybe Prelude.Text)
patchOperation_from = Lens.lens (\PatchOperation' {from} -> from) (\s@PatchOperation' {} a -> s {from = a} :: PatchOperation)

-- | An update operation to be performed with this PATCH request. The valid
-- value can be add, remove, replace or copy. Not all valid operations are
-- supported for a given resource. Support of the operations depends on
-- specific operational contexts. Attempts to apply an unsupported
-- operation on a resource will return an error message..
patchOperation_op :: Lens.Lens' PatchOperation (Prelude.Maybe Op)
patchOperation_op = Lens.lens (\PatchOperation' {op} -> op) (\s@PatchOperation' {} a -> s {op = a} :: PatchOperation)

-- | The op operation\'s target, as identified by a JSON Pointer value that
-- references a location within the targeted resource. For example, if the
-- target resource has an updateable property of {\"name\":\"value\"}, the
-- path for this property is \/name. If the name property value is a JSON
-- object (e.g., {\"name\": {\"child\/name\": \"child-value\"}}), the path
-- for the child\/name property will be \/name\/child~1name. Any slash
-- (\"\/\") character appearing in path names must be escaped with \"~1\",
-- as shown in the example above. Each op operation can have only one path
-- associated with it.
patchOperation_path :: Lens.Lens' PatchOperation (Prelude.Maybe Prelude.Text)
patchOperation_path = Lens.lens (\PatchOperation' {path} -> path) (\s@PatchOperation' {} a -> s {path = a} :: PatchOperation)

-- | The new target value of the update operation. It is applicable for the
-- add or replace operation. When using AWS CLI to update a property of a
-- JSON value, enclose the JSON object with a pair of single quotes in a
-- Linux shell, e.g., \'{\"a\": ...}\'.
patchOperation_value :: Lens.Lens' PatchOperation (Prelude.Maybe Prelude.Text)
patchOperation_value = Lens.lens (\PatchOperation' {value} -> value) (\s@PatchOperation' {} a -> s {value = a} :: PatchOperation)

instance Prelude.Hashable PatchOperation where
  hashWithSalt _salt PatchOperation' {..} =
    _salt `Prelude.hashWithSalt` from
      `Prelude.hashWithSalt` op
      `Prelude.hashWithSalt` path
      `Prelude.hashWithSalt` value

instance Prelude.NFData PatchOperation where
  rnf PatchOperation' {..} =
    Prelude.rnf from
      `Prelude.seq` Prelude.rnf op
      `Prelude.seq` Prelude.rnf path
      `Prelude.seq` Prelude.rnf value

instance Data.ToJSON PatchOperation where
  toJSON PatchOperation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("from" Data..=) Prelude.<$> from,
            ("op" Data..=) Prelude.<$> op,
            ("path" Data..=) Prelude.<$> path,
            ("value" Data..=) Prelude.<$> value
          ]
      )
