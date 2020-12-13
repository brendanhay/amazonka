{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.APICacheType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.APICacheType
  ( APICacheType
      ( APICacheType',
        T2Small,
        T2Medium,
        R4Large,
        R4XLarge,
        R42XLARGE,
        R44XLARGE,
        R48XLARGE,
        Small,
        Medium,
        Large,
        XLarge,
        Large2X,
        Large4X,
        Large8X,
        Large12X
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype APICacheType = APICacheType' Lude.Text
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

pattern T2Small :: APICacheType
pattern T2Small = APICacheType' "T2_SMALL"

pattern T2Medium :: APICacheType
pattern T2Medium = APICacheType' "T2_MEDIUM"

pattern R4Large :: APICacheType
pattern R4Large = APICacheType' "R4_LARGE"

pattern R4XLarge :: APICacheType
pattern R4XLarge = APICacheType' "R4_XLARGE"

pattern R42XLARGE :: APICacheType
pattern R42XLARGE = APICacheType' "R4_2XLARGE"

pattern R44XLARGE :: APICacheType
pattern R44XLARGE = APICacheType' "R4_4XLARGE"

pattern R48XLARGE :: APICacheType
pattern R48XLARGE = APICacheType' "R4_8XLARGE"

pattern Small :: APICacheType
pattern Small = APICacheType' "SMALL"

pattern Medium :: APICacheType
pattern Medium = APICacheType' "MEDIUM"

pattern Large :: APICacheType
pattern Large = APICacheType' "LARGE"

pattern XLarge :: APICacheType
pattern XLarge = APICacheType' "XLARGE"

pattern Large2X :: APICacheType
pattern Large2X = APICacheType' "LARGE_2X"

pattern Large4X :: APICacheType
pattern Large4X = APICacheType' "LARGE_4X"

pattern Large8X :: APICacheType
pattern Large8X = APICacheType' "LARGE_8X"

pattern Large12X :: APICacheType
pattern Large12X = APICacheType' "LARGE_12X"

{-# COMPLETE
  T2Small,
  T2Medium,
  R4Large,
  R4XLarge,
  R42XLARGE,
  R44XLARGE,
  R48XLARGE,
  Small,
  Medium,
  Large,
  XLarge,
  Large2X,
  Large4X,
  Large8X,
  Large12X,
  APICacheType'
  #-}
