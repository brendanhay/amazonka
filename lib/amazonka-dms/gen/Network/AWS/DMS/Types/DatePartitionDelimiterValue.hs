{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.DatePartitionDelimiterValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.DatePartitionDelimiterValue
  ( DatePartitionDelimiterValue
      ( DatePartitionDelimiterValue',
        DPDVSlash,
        DPDVUnderscore,
        DPDVDash,
        DPDVNone
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype DatePartitionDelimiterValue = DatePartitionDelimiterValue' Lude.Text
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

pattern DPDVSlash :: DatePartitionDelimiterValue
pattern DPDVSlash = DatePartitionDelimiterValue' "SLASH"

pattern DPDVUnderscore :: DatePartitionDelimiterValue
pattern DPDVUnderscore = DatePartitionDelimiterValue' "UNDERSCORE"

pattern DPDVDash :: DatePartitionDelimiterValue
pattern DPDVDash = DatePartitionDelimiterValue' "DASH"

pattern DPDVNone :: DatePartitionDelimiterValue
pattern DPDVNone = DatePartitionDelimiterValue' "NONE"

{-# COMPLETE
  DPDVSlash,
  DPDVUnderscore,
  DPDVDash,
  DPDVNone,
  DatePartitionDelimiterValue'
  #-}
