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
-- Module      : Network.AWS.Config.Types.MaximumExecutionFrequency
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.MaximumExecutionFrequency
  ( MaximumExecutionFrequency
      ( ..,
        MaximumExecutionFrequency_One_Hour,
        MaximumExecutionFrequency_Six_Hours,
        MaximumExecutionFrequency_Three_Hours,
        MaximumExecutionFrequency_Twelve_Hours,
        MaximumExecutionFrequency_TwentyFour_Hours
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype MaximumExecutionFrequency = MaximumExecutionFrequency'
  { fromMaximumExecutionFrequency ::
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

pattern MaximumExecutionFrequency_One_Hour :: MaximumExecutionFrequency
pattern MaximumExecutionFrequency_One_Hour = MaximumExecutionFrequency' "One_Hour"

pattern MaximumExecutionFrequency_Six_Hours :: MaximumExecutionFrequency
pattern MaximumExecutionFrequency_Six_Hours = MaximumExecutionFrequency' "Six_Hours"

pattern MaximumExecutionFrequency_Three_Hours :: MaximumExecutionFrequency
pattern MaximumExecutionFrequency_Three_Hours = MaximumExecutionFrequency' "Three_Hours"

pattern MaximumExecutionFrequency_Twelve_Hours :: MaximumExecutionFrequency
pattern MaximumExecutionFrequency_Twelve_Hours = MaximumExecutionFrequency' "Twelve_Hours"

pattern MaximumExecutionFrequency_TwentyFour_Hours :: MaximumExecutionFrequency
pattern MaximumExecutionFrequency_TwentyFour_Hours = MaximumExecutionFrequency' "TwentyFour_Hours"

{-# COMPLETE
  MaximumExecutionFrequency_One_Hour,
  MaximumExecutionFrequency_Six_Hours,
  MaximumExecutionFrequency_Three_Hours,
  MaximumExecutionFrequency_Twelve_Hours,
  MaximumExecutionFrequency_TwentyFour_Hours,
  MaximumExecutionFrequency'
  #-}
