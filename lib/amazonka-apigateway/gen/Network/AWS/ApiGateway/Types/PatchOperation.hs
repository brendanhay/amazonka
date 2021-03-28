{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.Types.PatchOperation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApiGateway.Types.PatchOperation
  ( PatchOperation (..)
  -- * Smart constructor
  , mkPatchOperation
  -- * Lenses
  , poFrom
  , poOp
  , poPath
  , poValue
  ) where

import qualified Network.AWS.ApiGateway.Types.Op as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A single patch operation to apply to the specified resource. Please refer to http://tools.ietf.org/html/rfc6902#section-4 for an explanation of how each operation is used.
--
-- /See:/ 'mkPatchOperation' smart constructor.
data PatchOperation = PatchOperation'
  { from :: Core.Maybe Core.Text
    -- ^ The @copy@ update operation's source as identified by a @JSON-Pointer@ value referencing the location within the targeted resource to copy the value from. For example, to promote a canary deployment, you copy the canary deployment ID to the affiliated deployment ID by calling a PATCH request on a 'Stage' resource with @"op":"copy"@ , @"from":"/canarySettings/deploymentId"@ and @"path":"/deploymentId"@ .
  , op :: Core.Maybe Types.Op
    -- ^ An update operation to be performed with this PATCH request. The valid value can be @add@ , @remove@ , @replace@ or @copy@ . Not all valid operations are supported for a given resource. Support of the operations depends on specific operational contexts. Attempts to apply an unsupported operation on a resource will return an error message.
  , path :: Core.Maybe Core.Text
    -- ^ The @op@ operation's target, as identified by a <https://tools.ietf.org/html/draft-ietf-appsawg-json-pointer-08 JSON Pointer> value that references a location within the targeted resource. For example, if the target resource has an updateable property of @{"name":"value"}@ , the path for this property is @/name@ . If the @name@ property value is a JSON object (e.g., @{"name": {"child/name": "child-value"}}@ ), the path for the @child/name@ property will be @/name/child~1name@ . Any slash ("/") character appearing in path names must be escaped with "~1", as shown in the example above. Each @op@ operation can have only one @path@ associated with it.
  , value :: Core.Maybe Core.Text
    -- ^ The new target value of the update operation. It is applicable for the @add@ or @replace@ operation. When using AWS CLI to update a property of a JSON value, enclose the JSON object with a pair of single quotes in a Linux shell, e.g., '{"a": ...}'. In a Windows shell, see <https://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PatchOperation' value with any optional fields omitted.
mkPatchOperation
    :: PatchOperation
mkPatchOperation
  = PatchOperation'{from = Core.Nothing, op = Core.Nothing,
                    path = Core.Nothing, value = Core.Nothing}

-- | The @copy@ update operation's source as identified by a @JSON-Pointer@ value referencing the location within the targeted resource to copy the value from. For example, to promote a canary deployment, you copy the canary deployment ID to the affiliated deployment ID by calling a PATCH request on a 'Stage' resource with @"op":"copy"@ , @"from":"/canarySettings/deploymentId"@ and @"path":"/deploymentId"@ .
--
-- /Note:/ Consider using 'from' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poFrom :: Lens.Lens' PatchOperation (Core.Maybe Core.Text)
poFrom = Lens.field @"from"
{-# INLINEABLE poFrom #-}
{-# DEPRECATED from "Use generic-lens or generic-optics with 'from' instead"  #-}

-- | An update operation to be performed with this PATCH request. The valid value can be @add@ , @remove@ , @replace@ or @copy@ . Not all valid operations are supported for a given resource. Support of the operations depends on specific operational contexts. Attempts to apply an unsupported operation on a resource will return an error message.
--
-- /Note:/ Consider using 'op' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poOp :: Lens.Lens' PatchOperation (Core.Maybe Types.Op)
poOp = Lens.field @"op"
{-# INLINEABLE poOp #-}
{-# DEPRECATED op "Use generic-lens or generic-optics with 'op' instead"  #-}

-- | The @op@ operation's target, as identified by a <https://tools.ietf.org/html/draft-ietf-appsawg-json-pointer-08 JSON Pointer> value that references a location within the targeted resource. For example, if the target resource has an updateable property of @{"name":"value"}@ , the path for this property is @/name@ . If the @name@ property value is a JSON object (e.g., @{"name": {"child/name": "child-value"}}@ ), the path for the @child/name@ property will be @/name/child~1name@ . Any slash ("/") character appearing in path names must be escaped with "~1", as shown in the example above. Each @op@ operation can have only one @path@ associated with it.
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poPath :: Lens.Lens' PatchOperation (Core.Maybe Core.Text)
poPath = Lens.field @"path"
{-# INLINEABLE poPath #-}
{-# DEPRECATED path "Use generic-lens or generic-optics with 'path' instead"  #-}

-- | The new target value of the update operation. It is applicable for the @add@ or @replace@ operation. When using AWS CLI to update a property of a JSON value, enclose the JSON object with a pair of single quotes in a Linux shell, e.g., '{"a": ...}'. In a Windows shell, see <https://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters> .
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poValue :: Lens.Lens' PatchOperation (Core.Maybe Core.Text)
poValue = Lens.field @"value"
{-# INLINEABLE poValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromJSON PatchOperation where
        toJSON PatchOperation{..}
          = Core.object
              (Core.catMaybes
                 [("from" Core..=) Core.<$> from, ("op" Core..=) Core.<$> op,
                  ("path" Core..=) Core.<$> path, ("value" Core..=) Core.<$> value])
