{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecretsManager.TagResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches one or more tags, each consisting of a key name and a value, to the specified secret. Tags are part of the secret's overall metadata, and are not associated with any specific version of the secret. This operation only appends tags to the existing list of tags. To remove tags, you must use 'UntagResource' .
--
-- The following basic restrictions apply to tags:
--
--     * Maximum number of tags per secret—50
--
--
--     * Maximum key length—127 Unicode characters in UTF-8
--
--
--     * Maximum value length—255 Unicode characters in UTF-8
--
--
--     * Tag keys and values are case sensitive.
--
--
--     * Do not use the @aws:@ prefix in your tag names or values because AWS reserves it for AWS use. You can't edit or delete tag names or values with this prefix. Tags with this prefix do not count against your tags per secret limit.
--
--
--     * If you use your tagging schema across multiple services and resources, remember other services might have restrictions on allowed characters. Generally allowed characters: letters, spaces, and numbers representable in UTF-8, plus the following special characters: + - = . _ : / @.
--
--
-- /Important:/ If you use tags as part of your security strategy, then adding or removing a tag can change permissions. If successfully completing this operation would result in you losing your permissions for this secret, then the operation is blocked and returns an Access Denied error.
-- __Minimum permissions__
-- To run this command, you must have the following permissions:
--
--     * secretsmanager:TagResource
--
--
-- __Related operations__
--
--     * To remove one or more tags from the collection attached to a secret, use 'UntagResource' .
--
--
--     * To view the list of tags attached to a secret, use 'DescribeSecret' .
module Network.AWS.SecretsManager.TagResource
  ( -- * Creating a request
    TagResource (..),
    mkTagResource,

    -- ** Request lenses
    trSecretId,
    trTags,

    -- * Destructuring the response
    TagResourceResponse (..),
    mkTagResourceResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SecretsManager.Types as Types

-- | /See:/ 'mkTagResource' smart constructor.
data TagResource = TagResource'
  { -- | The identifier for the secret that you want to attach tags to. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
    secretId :: Types.SecretId,
    -- | The tags to attach to the secret. Each element in the list consists of a @Key@ and a @Value@ .
    --
    -- This parameter to the API requires a JSON text string argument. For information on how to format a JSON parameter for the various command line tool environments, see <https://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters> in the /AWS CLI User Guide/ . For the AWS CLI, you can also use the syntax: @--Tags Key="Key1",Value="Value1",Key="Key2",Value="Value2"[,…]@
    tags :: [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TagResource' value with any optional fields omitted.
mkTagResource ::
  -- | 'secretId'
  Types.SecretId ->
  TagResource
mkTagResource secretId = TagResource' {secretId, tags = Core.mempty}

-- | The identifier for the secret that you want to attach tags to. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
--
-- /Note:/ Consider using 'secretId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trSecretId :: Lens.Lens' TagResource Types.SecretId
trSecretId = Lens.field @"secretId"
{-# DEPRECATED trSecretId "Use generic-lens or generic-optics with 'secretId' instead." #-}

-- | The tags to attach to the secret. Each element in the list consists of a @Key@ and a @Value@ .
--
-- This parameter to the API requires a JSON text string argument. For information on how to format a JSON parameter for the various command line tool environments, see <https://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters> in the /AWS CLI User Guide/ . For the AWS CLI, you can also use the syntax: @--Tags Key="Key1",Value="Value1",Key="Key2",Value="Value2"[,…]@
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trTags :: Lens.Lens' TagResource [Types.Tag]
trTags = Lens.field @"tags"
{-# DEPRECATED trTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON TagResource where
  toJSON TagResource {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("SecretId" Core..= secretId),
            Core.Just ("Tags" Core..= tags)
          ]
      )

instance Core.AWSRequest TagResource where
  type Rs TagResource = TagResourceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "secretsmanager.TagResource")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull TagResourceResponse'

-- | /See:/ 'mkTagResourceResponse' smart constructor.
data TagResourceResponse = TagResourceResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TagResourceResponse' value with any optional fields omitted.
mkTagResourceResponse ::
  TagResourceResponse
mkTagResourceResponse = TagResourceResponse'
