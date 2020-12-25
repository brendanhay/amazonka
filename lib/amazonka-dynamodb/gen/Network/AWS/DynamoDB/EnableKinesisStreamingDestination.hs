{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.EnableKinesisStreamingDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts table data replication to the specified Kinesis data stream at a timestamp chosen during the enable workflow. If this operation doesn't return results immediately, use DescribeKinesisStreamingDestination to check if streaming to the Kinesis data stream is ACTIVE.
module Network.AWS.DynamoDB.EnableKinesisStreamingDestination
  ( -- * Creating a request
    EnableKinesisStreamingDestination (..),
    mkEnableKinesisStreamingDestination,

    -- ** Request lenses
    eksdTableName,
    eksdStreamArn,

    -- * Destructuring the response
    Types.KinesisStreamingDestinationOutput (..),
    Types.mkKinesisStreamingDestinationOutput,

    -- ** Response lenses
    Types.ksdoDestinationStatus,
    Types.ksdoStreamArn,
    Types.ksdoTableName,
  )
where

import qualified Network.AWS.DynamoDB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkEnableKinesisStreamingDestination' smart constructor.
data EnableKinesisStreamingDestination = EnableKinesisStreamingDestination'
  { -- | The name of the DynamoDB table.
    tableName :: Types.TableName,
    -- | The ARN for a Kinesis data stream.
    streamArn :: Types.StreamArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnableKinesisStreamingDestination' value with any optional fields omitted.
mkEnableKinesisStreamingDestination ::
  -- | 'tableName'
  Types.TableName ->
  -- | 'streamArn'
  Types.StreamArn ->
  EnableKinesisStreamingDestination
mkEnableKinesisStreamingDestination tableName streamArn =
  EnableKinesisStreamingDestination' {tableName, streamArn}

-- | The name of the DynamoDB table.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eksdTableName :: Lens.Lens' EnableKinesisStreamingDestination Types.TableName
eksdTableName = Lens.field @"tableName"
{-# DEPRECATED eksdTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | The ARN for a Kinesis data stream.
--
-- /Note:/ Consider using 'streamArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eksdStreamArn :: Lens.Lens' EnableKinesisStreamingDestination Types.StreamArn
eksdStreamArn = Lens.field @"streamArn"
{-# DEPRECATED eksdStreamArn "Use generic-lens or generic-optics with 'streamArn' instead." #-}

instance Core.FromJSON EnableKinesisStreamingDestination where
  toJSON EnableKinesisStreamingDestination {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TableName" Core..= tableName),
            Core.Just ("StreamArn" Core..= streamArn)
          ]
      )

instance Core.AWSRequest EnableKinesisStreamingDestination where
  type
    Rs EnableKinesisStreamingDestination =
      Types.KinesisStreamingDestinationOutput
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "DynamoDB_20120810.EnableKinesisStreamingDestination"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.0")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveJSON (\s h x -> Core.eitherParseJSON x)
