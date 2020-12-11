-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.SplitType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.SplitType
  ( SplitType
      ( SplitType',
        STLine,
        STNone,
        STRecordIO,
        STTFRecord
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype SplitType = SplitType' Lude.Text
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

pattern STLine :: SplitType
pattern STLine = SplitType' "Line"

pattern STNone :: SplitType
pattern STNone = SplitType' "None"

pattern STRecordIO :: SplitType
pattern STRecordIO = SplitType' "RecordIO"

pattern STTFRecord :: SplitType
pattern STTFRecord = SplitType' "TFRecord"

{-# COMPLETE
  STLine,
  STNone,
  STRecordIO,
  STTFRecord,
  SplitType'
  #-}
