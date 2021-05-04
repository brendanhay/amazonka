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
-- Module      : Network.AWS.SageMaker.Types.SplitType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.SplitType
  ( SplitType
      ( ..,
        SplitType_Line,
        SplitType_None,
        SplitType_RecordIO,
        SplitType_TFRecord
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype SplitType = SplitType'
  { fromSplitType ::
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

pattern SplitType_Line :: SplitType
pattern SplitType_Line = SplitType' "Line"

pattern SplitType_None :: SplitType
pattern SplitType_None = SplitType' "None"

pattern SplitType_RecordIO :: SplitType
pattern SplitType_RecordIO = SplitType' "RecordIO"

pattern SplitType_TFRecord :: SplitType
pattern SplitType_TFRecord = SplitType' "TFRecord"

{-# COMPLETE
  SplitType_Line,
  SplitType_None,
  SplitType_RecordIO,
  SplitType_TFRecord,
  SplitType'
  #-}
