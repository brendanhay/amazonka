{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    dksdTableName,
    dksdStreamARN,

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
  { tableName ::
      Lude.Text,
    streamARN ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisableKinesisStreamingDestination' with the minimum fields required to make a request.
--
-- * 'streamARN' - The ARN for a Kinesis data stream.
-- * 'tableName' - The name of the DynamoDB table.
mkDisableKinesisStreamingDestination ::
  -- | 'tableName'
  Lude.Text ->
  -- | 'streamARN'
  Lude.Text ->
  DisableKinesisStreamingDestination
mkDisableKinesisStreamingDestination pTableName_ pStreamARN_ =
  DisableKinesisStreamingDestination'
    { tableName = pTableName_,
      streamARN = pStreamARN_
    }

-- | The name of the DynamoDB table.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dksdTableName :: Lens.Lens' DisableKinesisStreamingDestination Lude.Text
dksdTableName = Lens.lens (tableName :: DisableKinesisStreamingDestination -> Lude.Text) (\s a -> s {tableName = a} :: DisableKinesisStreamingDestination)
{-# DEPRECATED dksdTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | The ARN for a Kinesis data stream.
--
-- /Note:/ Consider using 'streamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dksdStreamARN :: Lens.Lens' DisableKinesisStreamingDestination Lude.Text
dksdStreamARN = Lens.lens (streamARN :: DisableKinesisStreamingDestination -> Lude.Text) (\s a -> s {streamARN = a} :: DisableKinesisStreamingDestination)
{-# DEPRECATED dksdStreamARN "Use generic-lens or generic-optics with 'streamARN' instead." #-}

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
          [ Lude.Just ("TableName" Lude..= tableName),
            Lude.Just ("StreamArn" Lude..= streamARN)
          ]
      )

instance Lude.ToPath DisableKinesisStreamingDestination where
  toPath = Lude.const "/"

instance Lude.ToQuery DisableKinesisStreamingDestination where
  toQuery = Lude.const Lude.mempty
