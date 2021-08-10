{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.Runtime
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.Runtime
  ( Runtime
      ( ..,
        Runtime_Dotnetcore1_0,
        Runtime_Dotnetcore2_0,
        Runtime_Dotnetcore2_1,
        Runtime_Dotnetcore3_1,
        Runtime_Go1_x,
        Runtime_Java11,
        Runtime_Java8,
        Runtime_Java8_al2,
        Runtime_Nodejs,
        Runtime_Nodejs10_x,
        Runtime_Nodejs12_x,
        Runtime_Nodejs14_x,
        Runtime_Nodejs4_3,
        Runtime_Nodejs4_3_edge,
        Runtime_Nodejs6_10,
        Runtime_Nodejs8_10,
        Runtime_Provided,
        Runtime_Provided_al2,
        Runtime_Python2_7,
        Runtime_Python3_6,
        Runtime_Python3_7,
        Runtime_Python3_8,
        Runtime_Ruby2_5,
        Runtime_Ruby2_7
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype Runtime = Runtime' {fromRuntime :: Core.Text}
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern Runtime_Dotnetcore1_0 :: Runtime
pattern Runtime_Dotnetcore1_0 = Runtime' "dotnetcore1.0"

pattern Runtime_Dotnetcore2_0 :: Runtime
pattern Runtime_Dotnetcore2_0 = Runtime' "dotnetcore2.0"

pattern Runtime_Dotnetcore2_1 :: Runtime
pattern Runtime_Dotnetcore2_1 = Runtime' "dotnetcore2.1"

pattern Runtime_Dotnetcore3_1 :: Runtime
pattern Runtime_Dotnetcore3_1 = Runtime' "dotnetcore3.1"

pattern Runtime_Go1_x :: Runtime
pattern Runtime_Go1_x = Runtime' "go1.x"

pattern Runtime_Java11 :: Runtime
pattern Runtime_Java11 = Runtime' "java11"

pattern Runtime_Java8 :: Runtime
pattern Runtime_Java8 = Runtime' "java8"

pattern Runtime_Java8_al2 :: Runtime
pattern Runtime_Java8_al2 = Runtime' "java8.al2"

pattern Runtime_Nodejs :: Runtime
pattern Runtime_Nodejs = Runtime' "nodejs"

pattern Runtime_Nodejs10_x :: Runtime
pattern Runtime_Nodejs10_x = Runtime' "nodejs10.x"

pattern Runtime_Nodejs12_x :: Runtime
pattern Runtime_Nodejs12_x = Runtime' "nodejs12.x"

pattern Runtime_Nodejs14_x :: Runtime
pattern Runtime_Nodejs14_x = Runtime' "nodejs14.x"

pattern Runtime_Nodejs4_3 :: Runtime
pattern Runtime_Nodejs4_3 = Runtime' "nodejs4.3"

pattern Runtime_Nodejs4_3_edge :: Runtime
pattern Runtime_Nodejs4_3_edge = Runtime' "nodejs4.3-edge"

pattern Runtime_Nodejs6_10 :: Runtime
pattern Runtime_Nodejs6_10 = Runtime' "nodejs6.10"

pattern Runtime_Nodejs8_10 :: Runtime
pattern Runtime_Nodejs8_10 = Runtime' "nodejs8.10"

pattern Runtime_Provided :: Runtime
pattern Runtime_Provided = Runtime' "provided"

pattern Runtime_Provided_al2 :: Runtime
pattern Runtime_Provided_al2 = Runtime' "provided.al2"

pattern Runtime_Python2_7 :: Runtime
pattern Runtime_Python2_7 = Runtime' "python2.7"

pattern Runtime_Python3_6 :: Runtime
pattern Runtime_Python3_6 = Runtime' "python3.6"

pattern Runtime_Python3_7 :: Runtime
pattern Runtime_Python3_7 = Runtime' "python3.7"

pattern Runtime_Python3_8 :: Runtime
pattern Runtime_Python3_8 = Runtime' "python3.8"

pattern Runtime_Ruby2_5 :: Runtime
pattern Runtime_Ruby2_5 = Runtime' "ruby2.5"

pattern Runtime_Ruby2_7 :: Runtime
pattern Runtime_Ruby2_7 = Runtime' "ruby2.7"

{-# COMPLETE
  Runtime_Dotnetcore1_0,
  Runtime_Dotnetcore2_0,
  Runtime_Dotnetcore2_1,
  Runtime_Dotnetcore3_1,
  Runtime_Go1_x,
  Runtime_Java11,
  Runtime_Java8,
  Runtime_Java8_al2,
  Runtime_Nodejs,
  Runtime_Nodejs10_x,
  Runtime_Nodejs12_x,
  Runtime_Nodejs14_x,
  Runtime_Nodejs4_3,
  Runtime_Nodejs4_3_edge,
  Runtime_Nodejs6_10,
  Runtime_Nodejs8_10,
  Runtime_Provided,
  Runtime_Provided_al2,
  Runtime_Python2_7,
  Runtime_Python3_6,
  Runtime_Python3_7,
  Runtime_Python3_8,
  Runtime_Ruby2_5,
  Runtime_Ruby2_7,
  Runtime'
  #-}
