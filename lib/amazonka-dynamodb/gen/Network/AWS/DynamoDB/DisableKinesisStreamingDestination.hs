{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.DisableKinesisStreamingDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops replication from the DynamoDB table to the Kinesis data stream. This is done without deleting either of the resources.
module Network.AWS.DynamoDB.DisableKinesisStreamingDestination
  ( -- * Creating a request
    DisableKinesisStreamingDestination (..),
    mkDisableKinesisStreamingDestination,

    -- ** Request lenses
    dksdfStreamARN,
    dksdfTableName,

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

-- | /See:/ 'mkDisableKinesisStreamingDestination' smart constructor.
data DisableKinesisStreamingDestination = DisableKinesisStreamingDestination'
  { -- | The ARN for a Kinesis data stream.
    streamARN :: Lude.Text,
    -- | The name of the DynamoDB table.
    tableName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisableKinesisStreamingDestination' with the minimum fields required to make a request.
--
-- * 'streamARN' - The ARN for a Kinesis data stream.
-- * 'tableName' - The name of the DynamoDB table.
mkDisableKinesisStreamingDestination ::
  -- | 'streamARN'
  Lude.Text ->
  -- | 'tableName'
  Lude.Text ->
  DisableKinesisStreamingDestination
mkDisableKinesisStreamingDestination pStreamARN_ pTableName_ =
  DisableKinesisStreamingDestination'
    { streamARN = pStreamARN_,
      tableName = pTableName_
    }

-- | The ARN for a Kinesis data stream.
--
-- /Note:/ Consider using 'streamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dksdfStreamARN :: Lens.Lens' DisableKinesisStreamingDestination Lude.Text
dksdfStreamARN = Lens.lens (streamARN :: DisableKinesisStreamingDestination -> Lude.Text) (\s a -> s {streamARN = a} :: DisableKinesisStreamingDestination)
{-# DEPRECATED dksdfStreamARN "Use generic-lens or generic-optics with 'streamARN' instead." #-}

-- | The name of the DynamoDB table.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dksdfTableName :: Lens.Lens' DisableKinesisStreamingDestination Lude.Text
dksdfTableName = Lens.lens (tableName :: DisableKinesisStreamingDestination -> Lude.Text) (\s a -> s {tableName = a} :: DisableKinesisStreamingDestination)
{-# DEPRECATED dksdfTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

instance Lude.AWSRequest DisableKinesisStreamingDestination where
  type
    Rs DisableKinesisStreamingDestination =
      KinesisStreamingDestinationOutput
  request = Req.postJSON dynamoDBService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders DisableKinesisStreamingDestination where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "DynamoDB_20120810.DisableKinesisStreamingDestination" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DisableKinesisStreamingDestination where
  toJSON DisableKinesisStreamingDestination' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("StreamArn" Lude..= streamARN),
            Lude.Just ("TableName" Lude..= tableName)
          ]
      )

instance Lude.ToPath DisableKinesisStreamingDestination where
  toPath = Lude.const "/"

instance Lude.ToQuery DisableKinesisStreamingDestination where
  toQuery = Lude.const Lude.mempty
