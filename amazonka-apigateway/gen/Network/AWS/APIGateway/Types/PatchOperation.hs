{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.APIGateway.Types.PatchOperation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.PatchOperation where

import Network.AWS.APIGateway.Types.Op
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A single patch operation to apply to the specified resource. Please
-- refer to http:\/\/tools.ietf.org\/html\/rfc6902#section-4 for an
-- explanation of how each operation is used.
--
-- /See:/ 'newPatchOperation' smart constructor.
data PatchOperation = PatchOperation'
  { -- | An update operation to be performed with this PATCH request. The valid
    -- value can be @add@, @remove@, @replace@ or @copy@. Not all valid
    -- operations are supported for a given resource. Support of the operations
    -- depends on specific operational contexts. Attempts to apply an
    -- unsupported operation on a resource will return an error message.
    op :: Prelude.Maybe Op,
    -- | The @copy@ update operation\'s source as identified by a @JSON-Pointer@
    -- value referencing the location within the targeted resource to copy the
    -- value from. For example, to promote a canary deployment, you copy the
    -- canary deployment ID to the affiliated deployment ID by calling a PATCH
    -- request on a Stage resource with @\"op\":\"copy\"@,
    -- @\"from\":\"\/canarySettings\/deploymentId\"@ and
    -- @\"path\":\"\/deploymentId\"@.
    from :: Prelude.Maybe Prelude.Text,
    -- | The new target value of the update operation. It is applicable for the
    -- @add@ or @replace@ operation. When using AWS CLI to update a property of
    -- a JSON value, enclose the JSON object with a pair of single quotes in a
    -- Linux shell, e.g., \'{\"a\": ...}\'. In a Windows shell, see
    -- <https://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters>.
    value :: Prelude.Maybe Prelude.Text,
    -- | The @op@ operation\'s target, as identified by a
    -- <https://tools.ietf.org/html/draft-ietf-appsawg-json-pointer-08 JSON Pointer>
    -- value that references a location within the targeted resource. For
    -- example, if the target resource has an updateable property of
    -- @{\"name\":\"value\"}@, the path for this property is @\/name@. If the
    -- @name@ property value is a JSON object (e.g.,
    -- @{\"name\": {\"child\/name\": \"child-value\"}}@), the path for the
    -- @child\/name@ property will be @\/name\/child~1name@. Any slash (\"\/\")
    -- character appearing in path names must be escaped with \"~1\", as shown
    -- in the example above. Each @op@ operation can have only one @path@
    -- associated with it.
    path :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PatchOperation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'op', 'patchOperation_op' - An update operation to be performed with this PATCH request. The valid
-- value can be @add@, @remove@, @replace@ or @copy@. Not all valid
-- operations are supported for a given resource. Support of the operations
-- depends on specific operational contexts. Attempts to apply an
-- unsupported operation on a resource will return an error message.
--
-- 'from', 'patchOperation_from' - The @copy@ update operation\'s source as identified by a @JSON-Pointer@
-- value referencing the location within the targeted resource to copy the
-- value from. For example, to promote a canary deployment, you copy the
-- canary deployment ID to the affiliated deployment ID by calling a PATCH
-- request on a Stage resource with @\"op\":\"copy\"@,
-- @\"from\":\"\/canarySettings\/deploymentId\"@ and
-- @\"path\":\"\/deploymentId\"@.
--
-- 'value', 'patchOperation_value' - The new target value of the update operation. It is applicable for the
-- @add@ or @replace@ operation. When using AWS CLI to update a property of
-- a JSON value, enclose the JSON object with a pair of single quotes in a
-- Linux shell, e.g., \'{\"a\": ...}\'. In a Windows shell, see
-- <https://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters>.
--
-- 'path', 'patchOperation_path' - The @op@ operation\'s target, as identified by a
-- <https://tools.ietf.org/html/draft-ietf-appsawg-json-pointer-08 JSON Pointer>
-- value that references a location within the targeted resource. For
-- example, if the target resource has an updateable property of
-- @{\"name\":\"value\"}@, the path for this property is @\/name@. If the
-- @name@ property value is a JSON object (e.g.,
-- @{\"name\": {\"child\/name\": \"child-value\"}}@), the path for the
-- @child\/name@ property will be @\/name\/child~1name@. Any slash (\"\/\")
-- character appearing in path names must be escaped with \"~1\", as shown
-- in the example above. Each @op@ operation can have only one @path@
-- associated with it.
newPatchOperation ::
  PatchOperation
newPatchOperation =
  PatchOperation'
    { op = Prelude.Nothing,
      from = Prelude.Nothing,
      value = Prelude.Nothing,
      path = Prelude.Nothing
    }

-- | An update operation to be performed with this PATCH request. The valid
-- value can be @add@, @remove@, @replace@ or @copy@. Not all valid
-- operations are supported for a given resource. Support of the operations
-- depends on specific operational contexts. Attempts to apply an
-- unsupported operation on a resource will return an error message.
patchOperation_op :: Lens.Lens' PatchOperation (Prelude.Maybe Op)
patchOperation_op = Lens.lens (\PatchOperation' {op} -> op) (\s@PatchOperation' {} a -> s {op = a} :: PatchOperation)

-- | The @copy@ update operation\'s source as identified by a @JSON-Pointer@
-- value referencing the location within the targeted resource to copy the
-- value from. For example, to promote a canary deployment, you copy the
-- canary deployment ID to the affiliated deployment ID by calling a PATCH
-- request on a Stage resource with @\"op\":\"copy\"@,
-- @\"from\":\"\/canarySettings\/deploymentId\"@ and
-- @\"path\":\"\/deploymentId\"@.
patchOperation_from :: Lens.Lens' PatchOperation (Prelude.Maybe Prelude.Text)
patchOperation_from = Lens.lens (\PatchOperation' {from} -> from) (\s@PatchOperation' {} a -> s {from = a} :: PatchOperation)

-- | The new target value of the update operation. It is applicable for the
-- @add@ or @replace@ operation. When using AWS CLI to update a property of
-- a JSON value, enclose the JSON object with a pair of single quotes in a
-- Linux shell, e.g., \'{\"a\": ...}\'. In a Windows shell, see
-- <https://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters>.
patchOperation_value :: Lens.Lens' PatchOperation (Prelude.Maybe Prelude.Text)
patchOperation_value = Lens.lens (\PatchOperation' {value} -> value) (\s@PatchOperation' {} a -> s {value = a} :: PatchOperation)

-- | The @op@ operation\'s target, as identified by a
-- <https://tools.ietf.org/html/draft-ietf-appsawg-json-pointer-08 JSON Pointer>
-- value that references a location within the targeted resource. For
-- example, if the target resource has an updateable property of
-- @{\"name\":\"value\"}@, the path for this property is @\/name@. If the
-- @name@ property value is a JSON object (e.g.,
-- @{\"name\": {\"child\/name\": \"child-value\"}}@), the path for the
-- @child\/name@ property will be @\/name\/child~1name@. Any slash (\"\/\")
-- character appearing in path names must be escaped with \"~1\", as shown
-- in the example above. Each @op@ operation can have only one @path@
-- associated with it.
patchOperation_path :: Lens.Lens' PatchOperation (Prelude.Maybe Prelude.Text)
patchOperation_path = Lens.lens (\PatchOperation' {path} -> path) (\s@PatchOperation' {} a -> s {path = a} :: PatchOperation)

instance Prelude.Hashable PatchOperation

instance Prelude.NFData PatchOperation

instance Prelude.ToJSON PatchOperation where
  toJSON PatchOperation' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("op" Prelude..=) Prelude.<$> op,
            ("from" Prelude..=) Prelude.<$> from,
            ("value" Prelude..=) Prelude.<$> value,
            ("path" Prelude..=) Prelude.<$> path
          ]
      )
