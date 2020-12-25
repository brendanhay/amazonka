{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.Runtime
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.Runtime
  ( Runtime
      ( Runtime',
        RuntimeNodejs,
        RuntimeNODEJS4_3,
        RuntimeNODEJS6_10,
        RuntimeNODEJS8_10,
        RuntimeNODEJS10_x,
        RuntimeNODEJS12_x,
        RuntimeJAVA8,
        RuntimeJAVA8_AL2,
        RuntimeJAVA11,
        RuntimePYTHON2_7,
        RuntimePYTHON3_6,
        RuntimePYTHON3_7,
        RuntimePYTHON3_8,
        RuntimeDOTNETCORE1_0,
        RuntimeDOTNETCORE2_0,
        RuntimeDOTNETCORE2_1,
        RuntimeDOTNETCORE3_1,
        RuntimeNODEJS4_3Edge,
        RuntimeGO1_x,
        RuntimeRUBY2_5,
        RuntimeRUBY2_7,
        RuntimeProvided,
        RuntimeProvided_AL2,
        fromRuntime
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype Runtime = Runtime' {fromRuntime :: Core.Text}
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

pattern RuntimeNodejs :: Runtime
pattern RuntimeNodejs = Runtime' "nodejs"

pattern RuntimeNODEJS4_3 :: Runtime
pattern RuntimeNODEJS4_3 = Runtime' "nodejs4.3"

pattern RuntimeNODEJS6_10 :: Runtime
pattern RuntimeNODEJS6_10 = Runtime' "nodejs6.10"

pattern RuntimeNODEJS8_10 :: Runtime
pattern RuntimeNODEJS8_10 = Runtime' "nodejs8.10"

pattern RuntimeNODEJS10_x :: Runtime
pattern RuntimeNODEJS10_x = Runtime' "nodejs10.x"

pattern RuntimeNODEJS12_x :: Runtime
pattern RuntimeNODEJS12_x = Runtime' "nodejs12.x"

pattern RuntimeJAVA8 :: Runtime
pattern RuntimeJAVA8 = Runtime' "java8"

pattern RuntimeJAVA8_AL2 :: Runtime
pattern RuntimeJAVA8_AL2 = Runtime' "java8.al2"

pattern RuntimeJAVA11 :: Runtime
pattern RuntimeJAVA11 = Runtime' "java11"

pattern RuntimePYTHON2_7 :: Runtime
pattern RuntimePYTHON2_7 = Runtime' "python2.7"

pattern RuntimePYTHON3_6 :: Runtime
pattern RuntimePYTHON3_6 = Runtime' "python3.6"

pattern RuntimePYTHON3_7 :: Runtime
pattern RuntimePYTHON3_7 = Runtime' "python3.7"

pattern RuntimePYTHON3_8 :: Runtime
pattern RuntimePYTHON3_8 = Runtime' "python3.8"

pattern RuntimeDOTNETCORE1_0 :: Runtime
pattern RuntimeDOTNETCORE1_0 = Runtime' "dotnetcore1.0"

pattern RuntimeDOTNETCORE2_0 :: Runtime
pattern RuntimeDOTNETCORE2_0 = Runtime' "dotnetcore2.0"

pattern RuntimeDOTNETCORE2_1 :: Runtime
pattern RuntimeDOTNETCORE2_1 = Runtime' "dotnetcore2.1"

pattern RuntimeDOTNETCORE3_1 :: Runtime
pattern RuntimeDOTNETCORE3_1 = Runtime' "dotnetcore3.1"

pattern RuntimeNODEJS4_3Edge :: Runtime
pattern RuntimeNODEJS4_3Edge = Runtime' "nodejs4.3-edge"

pattern RuntimeGO1_x :: Runtime
pattern RuntimeGO1_x = Runtime' "go1.x"

pattern RuntimeRUBY2_5 :: Runtime
pattern RuntimeRUBY2_5 = Runtime' "ruby2.5"

pattern RuntimeRUBY2_7 :: Runtime
pattern RuntimeRUBY2_7 = Runtime' "ruby2.7"

pattern RuntimeProvided :: Runtime
pattern RuntimeProvided = Runtime' "provided"

pattern RuntimeProvided_AL2 :: Runtime
pattern RuntimeProvided_AL2 = Runtime' "provided.al2"

{-# COMPLETE
  RuntimeNodejs,
  RuntimeNODEJS4_3,
  RuntimeNODEJS6_10,
  RuntimeNODEJS8_10,
  RuntimeNODEJS10_x,
  RuntimeNODEJS12_x,
  RuntimeJAVA8,
  RuntimeJAVA8_AL2,
  RuntimeJAVA11,
  RuntimePYTHON2_7,
  RuntimePYTHON3_6,
  RuntimePYTHON3_7,
  RuntimePYTHON3_8,
  RuntimeDOTNETCORE1_0,
  RuntimeDOTNETCORE2_0,
  RuntimeDOTNETCORE2_1,
  RuntimeDOTNETCORE3_1,
  RuntimeNODEJS4_3Edge,
  RuntimeGO1_x,
  RuntimeRUBY2_5,
  RuntimeRUBY2_7,
  RuntimeProvided,
  RuntimeProvided_AL2,
  Runtime'
  #-}
