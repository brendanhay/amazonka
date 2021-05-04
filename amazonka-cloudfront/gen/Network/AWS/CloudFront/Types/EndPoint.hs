{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.EndPoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.EndPoint where

import Network.AWS.CloudFront.Types.KinesisStreamConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about the Amazon Kinesis data stream where you are
-- sending real-time log data in a real-time log configuration.
--
-- /See:/ 'newEndPoint' smart constructor.
data EndPoint = EndPoint'
  { -- | Contains information about the Amazon Kinesis data stream where you are
    -- sending real-time log data.
    kinesisStreamConfig :: Prelude.Maybe KinesisStreamConfig,
    -- | The type of data stream where you are sending real-time log data. The
    -- only valid value is @Kinesis@.
    streamType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EndPoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kinesisStreamConfig', 'endPoint_kinesisStreamConfig' - Contains information about the Amazon Kinesis data stream where you are
-- sending real-time log data.
--
-- 'streamType', 'endPoint_streamType' - The type of data stream where you are sending real-time log data. The
-- only valid value is @Kinesis@.
newEndPoint ::
  -- | 'streamType'
  Prelude.Text ->
  EndPoint
newEndPoint pStreamType_ =
  EndPoint'
    { kinesisStreamConfig = Prelude.Nothing,
      streamType = pStreamType_
    }

-- | Contains information about the Amazon Kinesis data stream where you are
-- sending real-time log data.
endPoint_kinesisStreamConfig :: Lens.Lens' EndPoint (Prelude.Maybe KinesisStreamConfig)
endPoint_kinesisStreamConfig = Lens.lens (\EndPoint' {kinesisStreamConfig} -> kinesisStreamConfig) (\s@EndPoint' {} a -> s {kinesisStreamConfig = a} :: EndPoint)

-- | The type of data stream where you are sending real-time log data. The
-- only valid value is @Kinesis@.
endPoint_streamType :: Lens.Lens' EndPoint Prelude.Text
endPoint_streamType = Lens.lens (\EndPoint' {streamType} -> streamType) (\s@EndPoint' {} a -> s {streamType = a} :: EndPoint)

instance Prelude.FromXML EndPoint where
  parseXML x =
    EndPoint'
      Prelude.<$> (x Prelude..@? "KinesisStreamConfig")
      Prelude.<*> (x Prelude..@ "StreamType")

instance Prelude.Hashable EndPoint

instance Prelude.NFData EndPoint

instance Prelude.ToXML EndPoint where
  toXML EndPoint' {..} =
    Prelude.mconcat
      [ "KinesisStreamConfig"
          Prelude.@= kinesisStreamConfig,
        "StreamType" Prelude.@= streamType
      ]
