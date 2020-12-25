{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecretsManager.UntagResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes one or more tags from the specified secret.
--
-- This operation is idempotent. If a requested tag is not attached to the secret, no error is returned and the secret metadata is unchanged.
-- /Important:/ If you use tags as part of your security strategy, then removing a tag can change permissions. If successfully completing this operation would result in you losing your permissions for this secret, then the operation is blocked and returns an Access Denied error.
-- __Minimum permissions__
-- To run this command, you must have the following permissions:
--
--     * secretsmanager:UntagResource
--
--
-- __Related operations__
--
--     * To add one or more tags to the collection attached to a secret, use 'TagResource' .
--
--
--     * To view the list of tags attached to a secret, use 'DescribeSecret' .
module Network.AWS.SecretsManager.UntagResource
  ( -- * Creating a request
    UntagResource (..),
    mkUntagResource,

    -- ** Request lenses
    urSecretId,
    urTagKeys,

    -- * Destructuring the response
    UntagResourceResponse (..),
    mkUntagResourceResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SecretsManager.Types as Types

-- | /See:/ 'mkUntagResource' smart constructor.
data UntagResource = UntagResource'
  { -- | The identifier for the secret that you want to remove tags from. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
    secretId :: Types.SecretId,
    -- | A list of tag key names to remove from the secret. You don't specify the value. Both the key and its associated value are removed.
    --
    -- This parameter to the API requires a JSON text string argument. For information on how to format a JSON parameter for the various command line tool environments, see <https://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters> in the /AWS CLI User Guide/ .
    tagKeys :: [Types.TagKeyType]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UntagResource' value with any optional fields omitted.
mkUntagResource ::
  -- | 'secretId'
  Types.SecretId ->
  UntagResource
mkUntagResource secretId =
  UntagResource' {secretId, tagKeys = Core.mempty}

-- | The identifier for the secret that you want to remove tags from. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
--
-- /Note:/ Consider using 'secretId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urSecretId :: Lens.Lens' UntagResource Types.SecretId
urSecretId = Lens.field @"secretId"
{-# DEPRECATED urSecretId "Use generic-lens or generic-optics with 'secretId' instead." #-}

-- | A list of tag key names to remove from the secret. You don't specify the value. Both the key and its associated value are removed.
--
-- This parameter to the API requires a JSON text string argument. For information on how to format a JSON parameter for the various command line tool environments, see <https://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters> in the /AWS CLI User Guide/ .
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urTagKeys :: Lens.Lens' UntagResource [Types.TagKeyType]
urTagKeys = Lens.field @"tagKeys"
{-# DEPRECATED urTagKeys "Use generic-lens or generic-optics with 'tagKeys' instead." #-}

instance Core.FromJSON UntagResource where
  toJSON UntagResource {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("SecretId" Core..= secretId),
            Core.Just ("TagKeys" Core..= tagKeys)
          ]
      )

instance Core.AWSRequest UntagResource where
  type Rs UntagResource = UntagResourceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "secretsmanager.UntagResource")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull UntagResourceResponse'

-- | /See:/ 'mkUntagResourceResponse' smart constructor.
data UntagResourceResponse = UntagResourceResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UntagResourceResponse' value with any optional fields omitted.
mkUntagResourceResponse ::
  UntagResourceResponse
mkUntagResourceResponse = UntagResourceResponse'
