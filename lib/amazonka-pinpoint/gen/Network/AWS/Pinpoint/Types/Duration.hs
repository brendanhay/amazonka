{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.Duration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.Duration
  ( Duration
      ( Duration',
        Hr24,
        Day7,
        Day14,
        Day30
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype Duration = Duration' Lude.Text
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

pattern Hr24 :: Duration
pattern Hr24 = Duration' "HR_24"

pattern Day7 :: Duration
pattern Day7 = Duration' "DAY_7"

pattern Day14 :: Duration
pattern Day14 = Duration' "DAY_14"

pattern Day30 :: Duration
pattern Day30 = Duration' "DAY_30"

{-# COMPLETE
  Hr24,
  Day7,
  Day14,
  Day30,
  Duration'
  #-}
