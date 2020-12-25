{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.PublishVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a <https://docs.aws.amazon.com/lambda/latest/dg/versioning-aliases.html version> from the current code and configuration of a function. Use versions to create a snapshot of your function code and configuration that doesn't change.
--
-- AWS Lambda doesn't publish a version if the function's configuration and code haven't changed since the last version. Use 'UpdateFunctionCode' or 'UpdateFunctionConfiguration' to update the function before publishing a version.
-- Clients can invoke versions directly or with an alias. To create an alias, use 'CreateAlias' .
module Network.AWS.Lambda.PublishVersion
  ( -- * Creating a request
    PublishVersion (..),
    mkPublishVersion,

    -- ** Request lenses
    pvFunctionName,
    pvCodeSha256,
    pvDescription,
    pvRevisionId,

    -- * Destructuring the response
    Types.FunctionConfiguration (..),
    Types.mkFunctionConfiguration,

    -- ** Response lenses
    Types.fcCodeSha256,
    Types.fcCodeSize,
    Types.fcDeadLetterConfig,
    Types.fcDescription,
    Types.fcEnvironment,
    Types.fcFileSystemConfigs,
    Types.fcFunctionArn,
    Types.fcFunctionName,
    Types.fcHandler,
    Types.fcKMSKeyArn,
    Types.fcLastModified,
    Types.fcLastUpdateStatus,
    Types.fcLastUpdateStatusReason,
    Types.fcLastUpdateStatusReasonCode,
    Types.fcLayers,
    Types.fcMasterArn,
    Types.fcMemorySize,
    Types.fcRevisionId,
    Types.fcRole,
    Types.fcRuntime,
    Types.fcSigningJobArn,
    Types.fcSigningProfileVersionArn,
    Types.fcState,
    Types.fcStateReason,
    Types.fcStateReasonCode,
    Types.fcTimeout,
    Types.fcTracingConfig,
    Types.fcVersion,
    Types.fcVpcConfig,
  )
where

import qualified Network.AWS.Lambda.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPublishVersion' smart constructor.
data PublishVersion = PublishVersion'
  { -- | The name of the Lambda function.
    --
    -- __Name formats__
    --
    --     * __Function name__ - @MyFunction@ .
    --
    --
    --     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@ .
    --
    --
    --     * __Partial ARN__ - @123456789012:function:MyFunction@ .
    --
    --
    -- The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
    functionName :: Types.FunctionName,
    -- | Only publish a version if the hash value matches the value that's specified. Use this option to avoid publishing a version if the function code has changed since you last updated it. You can get the hash for the version that you uploaded from the output of 'UpdateFunctionCode' .
    codeSha256 :: Core.Maybe Types.String,
    -- | A description for the version to override the description in the function configuration.
    description :: Core.Maybe Types.Description,
    -- | Only update the function if the revision ID matches the ID that's specified. Use this option to avoid publishing a version if the function configuration has changed since you last updated it.
    revisionId :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PublishVersion' value with any optional fields omitted.
mkPublishVersion ::
  -- | 'functionName'
  Types.FunctionName ->
  PublishVersion
mkPublishVersion functionName =
  PublishVersion'
    { functionName,
      codeSha256 = Core.Nothing,
      description = Core.Nothing,
      revisionId = Core.Nothing
    }

-- | The name of the Lambda function.
--
-- __Name formats__
--
--     * __Function name__ - @MyFunction@ .
--
--
--     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@ .
--
--
--     * __Partial ARN__ - @123456789012:function:MyFunction@ .
--
--
-- The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
--
-- /Note:/ Consider using 'functionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvFunctionName :: Lens.Lens' PublishVersion Types.FunctionName
pvFunctionName = Lens.field @"functionName"
{-# DEPRECATED pvFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

-- | Only publish a version if the hash value matches the value that's specified. Use this option to avoid publishing a version if the function code has changed since you last updated it. You can get the hash for the version that you uploaded from the output of 'UpdateFunctionCode' .
--
-- /Note:/ Consider using 'codeSha256' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvCodeSha256 :: Lens.Lens' PublishVersion (Core.Maybe Types.String)
pvCodeSha256 = Lens.field @"codeSha256"
{-# DEPRECATED pvCodeSha256 "Use generic-lens or generic-optics with 'codeSha256' instead." #-}

-- | A description for the version to override the description in the function configuration.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvDescription :: Lens.Lens' PublishVersion (Core.Maybe Types.Description)
pvDescription = Lens.field @"description"
{-# DEPRECATED pvDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Only update the function if the revision ID matches the ID that's specified. Use this option to avoid publishing a version if the function configuration has changed since you last updated it.
--
-- /Note:/ Consider using 'revisionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvRevisionId :: Lens.Lens' PublishVersion (Core.Maybe Types.String)
pvRevisionId = Lens.field @"revisionId"
{-# DEPRECATED pvRevisionId "Use generic-lens or generic-optics with 'revisionId' instead." #-}

instance Core.FromJSON PublishVersion where
  toJSON PublishVersion {..} =
    Core.object
      ( Core.catMaybes
          [ ("CodeSha256" Core..=) Core.<$> codeSha256,
            ("Description" Core..=) Core.<$> description,
            ("RevisionId" Core..=) Core.<$> revisionId
          ]
      )

instance Core.AWSRequest PublishVersion where
  type Rs PublishVersion = Types.FunctionConfiguration
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/2015-03-31/functions/" Core.<> (Core.toText functionName)
                Core.<> ("/versions")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveJSON (\s h x -> Core.eitherParseJSON x)
