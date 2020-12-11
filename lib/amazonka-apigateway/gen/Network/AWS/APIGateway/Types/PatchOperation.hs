-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.PatchOperation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.PatchOperation
  ( PatchOperation (..),

    -- * Smart constructor
    mkPatchOperation,

    -- * Lenses
    poOp,
    poPath,
    poValue,
    poFrom,
  )
where

import Network.AWS.APIGateway.Types.Op
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A single patch operation to apply to the specified resource. Please refer to http://tools.ietf.org/html/rfc6902#section-4 for an explanation of how each operation is used.
--
-- /See:/ 'mkPatchOperation' smart constructor.
data PatchOperation = PatchOperation'
  { op :: Lude.Maybe Op,
    path :: Lude.Maybe Lude.Text,
    value :: Lude.Maybe Lude.Text,
    from :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PatchOperation' with the minimum fields required to make a request.
--
-- * 'from' - The @copy@ update operation's source as identified by a @JSON-Pointer@ value referencing the location within the targeted resource to copy the value from. For example, to promote a canary deployment, you copy the canary deployment ID to the affiliated deployment ID by calling a PATCH request on a 'Stage' resource with @"op":"copy"@ , @"from":"/canarySettings/deploymentId"@ and @"path":"/deploymentId"@ .
-- * 'op' - An update operation to be performed with this PATCH request. The valid value can be @add@ , @remove@ , @replace@ or @copy@ . Not all valid operations are supported for a given resource. Support of the operations depends on specific operational contexts. Attempts to apply an unsupported operation on a resource will return an error message.
-- * 'path' - The @op@ operation's target, as identified by a <https://tools.ietf.org/html/draft-ietf-appsawg-json-pointer-08 JSON Pointer> value that references a location within the targeted resource. For example, if the target resource has an updateable property of @{"name":"value"}@ , the path for this property is @/name@ . If the @name@ property value is a JSON object (e.g., @{"name": {"child/name": "child-value"}}@ ), the path for the @child/name@ property will be @/name/child~1name@ . Any slash ("/") character appearing in path names must be escaped with "~1", as shown in the example above. Each @op@ operation can have only one @path@ associated with it.
-- * 'value' - The new target value of the update operation. It is applicable for the @add@ or @replace@ operation. When using AWS CLI to update a property of a JSON value, enclose the JSON object with a pair of single quotes in a Linux shell, e.g., '{"a": ...}'. In a Windows shell, see <https://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters> .
mkPatchOperation ::
  PatchOperation
mkPatchOperation =
  PatchOperation'
    { op = Lude.Nothing,
      path = Lude.Nothing,
      value = Lude.Nothing,
      from = Lude.Nothing
    }

-- | An update operation to be performed with this PATCH request. The valid value can be @add@ , @remove@ , @replace@ or @copy@ . Not all valid operations are supported for a given resource. Support of the operations depends on specific operational contexts. Attempts to apply an unsupported operation on a resource will return an error message.
--
-- /Note:/ Consider using 'op' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poOp :: Lens.Lens' PatchOperation (Lude.Maybe Op)
poOp = Lens.lens (op :: PatchOperation -> Lude.Maybe Op) (\s a -> s {op = a} :: PatchOperation)
{-# DEPRECATED poOp "Use generic-lens or generic-optics with 'op' instead." #-}

-- | The @op@ operation's target, as identified by a <https://tools.ietf.org/html/draft-ietf-appsawg-json-pointer-08 JSON Pointer> value that references a location within the targeted resource. For example, if the target resource has an updateable property of @{"name":"value"}@ , the path for this property is @/name@ . If the @name@ property value is a JSON object (e.g., @{"name": {"child/name": "child-value"}}@ ), the path for the @child/name@ property will be @/name/child~1name@ . Any slash ("/") character appearing in path names must be escaped with "~1", as shown in the example above. Each @op@ operation can have only one @path@ associated with it.
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poPath :: Lens.Lens' PatchOperation (Lude.Maybe Lude.Text)
poPath = Lens.lens (path :: PatchOperation -> Lude.Maybe Lude.Text) (\s a -> s {path = a} :: PatchOperation)
{-# DEPRECATED poPath "Use generic-lens or generic-optics with 'path' instead." #-}

-- | The new target value of the update operation. It is applicable for the @add@ or @replace@ operation. When using AWS CLI to update a property of a JSON value, enclose the JSON object with a pair of single quotes in a Linux shell, e.g., '{"a": ...}'. In a Windows shell, see <https://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters> .
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poValue :: Lens.Lens' PatchOperation (Lude.Maybe Lude.Text)
poValue = Lens.lens (value :: PatchOperation -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: PatchOperation)
{-# DEPRECATED poValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The @copy@ update operation's source as identified by a @JSON-Pointer@ value referencing the location within the targeted resource to copy the value from. For example, to promote a canary deployment, you copy the canary deployment ID to the affiliated deployment ID by calling a PATCH request on a 'Stage' resource with @"op":"copy"@ , @"from":"/canarySettings/deploymentId"@ and @"path":"/deploymentId"@ .
--
-- /Note:/ Consider using 'from' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poFrom :: Lens.Lens' PatchOperation (Lude.Maybe Lude.Text)
poFrom = Lens.lens (from :: PatchOperation -> Lude.Maybe Lude.Text) (\s a -> s {from = a} :: PatchOperation)
{-# DEPRECATED poFrom "Use generic-lens or generic-optics with 'from' instead." #-}

instance Lude.ToJSON PatchOperation where
  toJSON PatchOperation' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("op" Lude..=) Lude.<$> op,
            ("path" Lude..=) Lude.<$> path,
            ("value" Lude..=) Lude.<$> value,
            ("from" Lude..=) Lude.<$> from
          ]
      )
