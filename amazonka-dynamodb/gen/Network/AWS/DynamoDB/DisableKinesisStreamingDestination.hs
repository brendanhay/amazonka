{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.DisableKinesisStreamingDestination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops replication from the DynamoDB table to the Kinesis data stream.
-- This is done without deleting either of the resources.
module Network.AWS.DynamoDB.DisableKinesisStreamingDestination
  ( -- * Creating a Request
    DisableKinesisStreamingDestination (..),
    newDisableKinesisStreamingDestination,

    -- * Request Lenses
    disableKinesisStreamingDestination_tableName,
    disableKinesisStreamingDestination_streamArn,

    -- * Destructuring the Response
    KinesisStreamingDestinationOutput (..),
    newKinesisStreamingDestinationOutput,

    -- * Response Lenses
    kinesisStreamingDestinationOutput_tableName,
    kinesisStreamingDestinationOutput_streamArn,
    kinesisStreamingDestinationOutput_destinationStatus,
  )
where

import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisableKinesisStreamingDestination' smart constructor.
data DisableKinesisStreamingDestination = DisableKinesisStreamingDestination'
  { -- | The name of the DynamoDB table.
    tableName :: Prelude.Text,
    -- | The ARN for a Kinesis data stream.
    streamArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DisableKinesisStreamingDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tableName', 'disableKinesisStreamingDestination_tableName' - The name of the DynamoDB table.
--
-- 'streamArn', 'disableKinesisStreamingDestination_streamArn' - The ARN for a Kinesis data stream.
newDisableKinesisStreamingDestination ::
  -- | 'tableName'
  Prelude.Text ->
  -- | 'streamArn'
  Prelude.Text ->
  DisableKinesisStreamingDestination
newDisableKinesisStreamingDestination
  pTableName_
  pStreamArn_ =
    DisableKinesisStreamingDestination'
      { tableName =
          pTableName_,
        streamArn = pStreamArn_
      }

-- | The name of the DynamoDB table.
disableKinesisStreamingDestination_tableName :: Lens.Lens' DisableKinesisStreamingDestination Prelude.Text
disableKinesisStreamingDestination_tableName = Lens.lens (\DisableKinesisStreamingDestination' {tableName} -> tableName) (\s@DisableKinesisStreamingDestination' {} a -> s {tableName = a} :: DisableKinesisStreamingDestination)

-- | The ARN for a Kinesis data stream.
disableKinesisStreamingDestination_streamArn :: Lens.Lens' DisableKinesisStreamingDestination Prelude.Text
disableKinesisStreamingDestination_streamArn = Lens.lens (\DisableKinesisStreamingDestination' {streamArn} -> streamArn) (\s@DisableKinesisStreamingDestination' {} a -> s {streamArn = a} :: DisableKinesisStreamingDestination)

instance
  Prelude.AWSRequest
    DisableKinesisStreamingDestination
  where
  type
    Rs DisableKinesisStreamingDestination =
      KinesisStreamingDestinationOutput
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Prelude.eitherParseJSON x)

instance
  Prelude.Hashable
    DisableKinesisStreamingDestination

instance
  Prelude.NFData
    DisableKinesisStreamingDestination

instance
  Prelude.ToHeaders
    DisableKinesisStreamingDestination
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "DynamoDB_20120810.DisableKinesisStreamingDestination" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.0" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    DisableKinesisStreamingDestination
  where
  toJSON DisableKinesisStreamingDestination' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("TableName" Prelude..= tableName),
            Prelude.Just ("StreamArn" Prelude..= streamArn)
          ]
      )

instance
  Prelude.ToPath
    DisableKinesisStreamingDestination
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DisableKinesisStreamingDestination
  where
  toQuery = Prelude.const Prelude.mempty
