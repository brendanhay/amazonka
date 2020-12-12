{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.SnowballCapacity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.SnowballCapacity
  ( SnowballCapacity
      ( SnowballCapacity',
        NoPreference,
        T100,
        T42,
        T50,
        T8,
        T80,
        T98
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype SnowballCapacity = SnowballCapacity' Lude.Text
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

pattern NoPreference :: SnowballCapacity
pattern NoPreference = SnowballCapacity' "NoPreference"

pattern T100 :: SnowballCapacity
pattern T100 = SnowballCapacity' "T100"

pattern T42 :: SnowballCapacity
pattern T42 = SnowballCapacity' "T42"

pattern T50 :: SnowballCapacity
pattern T50 = SnowballCapacity' "T50"

pattern T8 :: SnowballCapacity
pattern T8 = SnowballCapacity' "T8"

pattern T80 :: SnowballCapacity
pattern T80 = SnowballCapacity' "T80"

pattern T98 :: SnowballCapacity
pattern T98 = SnowballCapacity' "T98"

{-# COMPLETE
  NoPreference,
  T100,
  T42,
  T50,
  T8,
  T80,
  T98,
  SnowballCapacity'
  #-}
