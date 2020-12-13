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
    eksdStreamARN,
    eksdTableName,

    -- * Destructuring the response
    KinesisStreamingDestinationOutput (..),
    mkKinesisStreamingDestinationOutput,

    -- ** Response lenses
    ksdoDestinationStatus,
    ksdoStreamARN,
    ksdoTableName,
  )
where

import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkEnableKinesisStreamingDestination' smart constructor.
data EnableKinesisStreamingDestination = EnableKinesisStreamingDestination'
  { -- | The ARN for a Kinesis data stream.
    streamARN :: Lude.Text,
    -- | The name of the DynamoDB table.
    tableName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnableKinesisStreamingDestination' with the minimum fields required to make a request.
--
-- * 'streamARN' - The ARN for a Kinesis data stream.
-- * 'tableName' - The name of the DynamoDB table.
mkEnableKinesisStreamingDestination ::
  -- | 'streamARN'
  Lude.Text ->
  -- | 'tableName'
  Lude.Text ->
  EnableKinesisStreamingDestination
mkEnableKinesisStreamingDestination pStreamARN_ pTableName_ =
  EnableKinesisStreamingDestination'
    { streamARN = pStreamARN_,
      tableName = pTableName_
    }

-- | The ARN for a Kinesis data stream.
--
-- /Note:/ Consider using 'streamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eksdStreamARN :: Lens.Lens' EnableKinesisStreamingDestination Lude.Text
eksdStreamARN = Lens.lens (streamARN :: EnableKinesisStreamingDestination -> Lude.Text) (\s a -> s {streamARN = a} :: EnableKinesisStreamingDestination)
{-# DEPRECATED eksdStreamARN "Use generic-lens or generic-optics with 'streamARN' instead." #-}

-- | The name of the DynamoDB table.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eksdTableName :: Lens.Lens' EnableKinesisStreamingDestination Lude.Text
eksdTableName = Lens.lens (tableName :: EnableKinesisStreamingDestination -> Lude.Text) (\s a -> s {tableName = a} :: EnableKinesisStreamingDestination)
{-# DEPRECATED eksdTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

instance Lude.AWSRequest EnableKinesisStreamingDestination where
  type
    Rs EnableKinesisStreamingDestination =
      KinesisStreamingDestinationOutput
  request = Req.postJSON dynamoDBService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders EnableKinesisStreamingDestination where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "DynamoDB_20120810.EnableKinesisStreamingDestination" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON EnableKinesisStreamingDestination where
  toJSON EnableKinesisStreamingDestination' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("StreamArn" Lude..= streamARN),
            Lude.Just ("TableName" Lude..= tableName)
          ]
      )

instance Lude.ToPath EnableKinesisStreamingDestination where
  toPath = Lude.const "/"

instance Lude.ToQuery EnableKinesisStreamingDestination where
  toQuery = Lude.const Lude.mempty
