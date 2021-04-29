{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.StorageType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.StorageType
  ( StorageType
      ( ..,
        StorageType_KINESIS_FIREHOSE,
        StorageType_KINESIS_STREAM,
        StorageType_KINESIS_VIDEO_STREAM,
        StorageType_S3
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype StorageType = StorageType'
  { fromStorageType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern StorageType_KINESIS_FIREHOSE :: StorageType
pattern StorageType_KINESIS_FIREHOSE = StorageType' "KINESIS_FIREHOSE"

pattern StorageType_KINESIS_STREAM :: StorageType
pattern StorageType_KINESIS_STREAM = StorageType' "KINESIS_STREAM"

pattern StorageType_KINESIS_VIDEO_STREAM :: StorageType
pattern StorageType_KINESIS_VIDEO_STREAM = StorageType' "KINESIS_VIDEO_STREAM"

pattern StorageType_S3 :: StorageType
pattern StorageType_S3 = StorageType' "S3"

{-# COMPLETE
  StorageType_KINESIS_FIREHOSE,
  StorageType_KINESIS_STREAM,
  StorageType_KINESIS_VIDEO_STREAM,
  StorageType_S3,
  StorageType'
  #-}
