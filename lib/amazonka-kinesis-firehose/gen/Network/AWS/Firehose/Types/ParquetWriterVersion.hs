{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.ParquetWriterVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.ParquetWriterVersion
  ( ParquetWriterVersion
      ( ParquetWriterVersion',
        ParquetWriterVersionV1,
        ParquetWriterVersionV2,
        fromParquetWriterVersion
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ParquetWriterVersion = ParquetWriterVersion'
  { fromParquetWriterVersion ::
      Core.Text
  }
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

pattern ParquetWriterVersionV1 :: ParquetWriterVersion
pattern ParquetWriterVersionV1 = ParquetWriterVersion' "V1"

pattern ParquetWriterVersionV2 :: ParquetWriterVersion
pattern ParquetWriterVersionV2 = ParquetWriterVersion' "V2"

{-# COMPLETE
  ParquetWriterVersionV1,
  ParquetWriterVersionV2,
  ParquetWriterVersion'
  #-}
