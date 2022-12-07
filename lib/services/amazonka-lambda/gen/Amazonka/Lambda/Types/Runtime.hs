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
-- Module      : Amazonka.Lambda.Types.Runtime
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types.Runtime
  ( Runtime
      ( ..,
        Runtime_Dotnet6,
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
        Runtime_Nodejs16_x,
        Runtime_Nodejs18_x,
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
        Runtime_Python3_9,
        Runtime_Ruby2_5,
        Runtime_Ruby2_7
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype Runtime = Runtime' {fromRuntime :: Data.Text}
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern Runtime_Dotnet6 :: Runtime
pattern Runtime_Dotnet6 = Runtime' "dotnet6"

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

pattern Runtime_Nodejs16_x :: Runtime
pattern Runtime_Nodejs16_x = Runtime' "nodejs16.x"

pattern Runtime_Nodejs18_x :: Runtime
pattern Runtime_Nodejs18_x = Runtime' "nodejs18.x"

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

pattern Runtime_Python3_9 :: Runtime
pattern Runtime_Python3_9 = Runtime' "python3.9"

pattern Runtime_Ruby2_5 :: Runtime
pattern Runtime_Ruby2_5 = Runtime' "ruby2.5"

pattern Runtime_Ruby2_7 :: Runtime
pattern Runtime_Ruby2_7 = Runtime' "ruby2.7"

{-# COMPLETE
  Runtime_Dotnet6,
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
  Runtime_Nodejs16_x,
  Runtime_Nodejs18_x,
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
  Runtime_Python3_9,
  Runtime_Ruby2_5,
  Runtime_Ruby2_7,
  Runtime'
  #-}
