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
        V1,
        V2
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ParquetWriterVersion = ParquetWriterVersion' Lude.Text
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

pattern V1 :: ParquetWriterVersion
pattern V1 = ParquetWriterVersion' "V1"

pattern V2 :: ParquetWriterVersion
pattern V2 = ParquetWriterVersion' "V2"

{-# COMPLETE
  V1,
  V2,
  ParquetWriterVersion'
  #-}
