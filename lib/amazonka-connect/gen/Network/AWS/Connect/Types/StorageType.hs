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
        StorageTypeS3,
        StorageTypeKinesisVideoStream,
        StorageTypeKinesisStream,
        StorageTypeKinesisFirehose,
        fromStorageType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype StorageType = StorageType' {fromStorageType :: Core.Text}
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern StorageTypeS3 :: StorageType
pattern StorageTypeS3 = StorageType' "S3"

pattern StorageTypeKinesisVideoStream :: StorageType
pattern StorageTypeKinesisVideoStream = StorageType' "KINESIS_VIDEO_STREAM"

pattern StorageTypeKinesisStream :: StorageType
pattern StorageTypeKinesisStream = StorageType' "KINESIS_STREAM"

pattern StorageTypeKinesisFirehose :: StorageType
pattern StorageTypeKinesisFirehose = StorageType' "KINESIS_FIREHOSE"

{-# COMPLETE
  StorageTypeS3,
  StorageTypeKinesisVideoStream,
  StorageTypeKinesisStream,
  StorageTypeKinesisFirehose,
  StorageType'
  #-}
