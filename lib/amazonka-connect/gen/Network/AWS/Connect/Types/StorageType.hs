{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.StorageType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.StorageType
  ( StorageType
      ( StorageType',
        S3,
        KinesisVideoStream,
        KinesisStream,
        KinesisFirehose
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype StorageType = StorageType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern S3 :: StorageType
pattern S3 = StorageType' "S3"

pattern KinesisVideoStream :: StorageType
pattern KinesisVideoStream = StorageType' "KINESIS_VIDEO_STREAM"

pattern KinesisStream :: StorageType
pattern KinesisStream = StorageType' "KINESIS_STREAM"

pattern KinesisFirehose :: StorageType
pattern KinesisFirehose = StorageType' "KINESIS_FIREHOSE"

{-# COMPLETE
  S3,
  KinesisVideoStream,
  KinesisStream,
  KinesisFirehose,
  StorageType'
  #-}
