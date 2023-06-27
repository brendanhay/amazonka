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
-- Module      : Amazonka.Inspector2.Types.Runtime
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.Runtime
  ( Runtime
      ( ..,
        Runtime_GO_1_X,
        Runtime_JAVA_11,
        Runtime_JAVA_17,
        Runtime_JAVA_8,
        Runtime_JAVA_8_AL2,
        Runtime_NODEJS,
        Runtime_NODEJS_12_X,
        Runtime_NODEJS_14_X,
        Runtime_NODEJS_16_X,
        Runtime_NODEJS_18_X,
        Runtime_PYTHON_3_10,
        Runtime_PYTHON_3_7,
        Runtime_PYTHON_3_8,
        Runtime_PYTHON_3_9,
        Runtime_UNSUPPORTED
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

pattern Runtime_GO_1_X :: Runtime
pattern Runtime_GO_1_X = Runtime' "GO_1_X"

pattern Runtime_JAVA_11 :: Runtime
pattern Runtime_JAVA_11 = Runtime' "JAVA_11"

pattern Runtime_JAVA_17 :: Runtime
pattern Runtime_JAVA_17 = Runtime' "JAVA_17"

pattern Runtime_JAVA_8 :: Runtime
pattern Runtime_JAVA_8 = Runtime' "JAVA_8"

pattern Runtime_JAVA_8_AL2 :: Runtime
pattern Runtime_JAVA_8_AL2 = Runtime' "JAVA_8_AL2"

pattern Runtime_NODEJS :: Runtime
pattern Runtime_NODEJS = Runtime' "NODEJS"

pattern Runtime_NODEJS_12_X :: Runtime
pattern Runtime_NODEJS_12_X = Runtime' "NODEJS_12_X"

pattern Runtime_NODEJS_14_X :: Runtime
pattern Runtime_NODEJS_14_X = Runtime' "NODEJS_14_X"

pattern Runtime_NODEJS_16_X :: Runtime
pattern Runtime_NODEJS_16_X = Runtime' "NODEJS_16_X"

pattern Runtime_NODEJS_18_X :: Runtime
pattern Runtime_NODEJS_18_X = Runtime' "NODEJS_18_X"

pattern Runtime_PYTHON_3_10 :: Runtime
pattern Runtime_PYTHON_3_10 = Runtime' "PYTHON_3_10"

pattern Runtime_PYTHON_3_7 :: Runtime
pattern Runtime_PYTHON_3_7 = Runtime' "PYTHON_3_7"

pattern Runtime_PYTHON_3_8 :: Runtime
pattern Runtime_PYTHON_3_8 = Runtime' "PYTHON_3_8"

pattern Runtime_PYTHON_3_9 :: Runtime
pattern Runtime_PYTHON_3_9 = Runtime' "PYTHON_3_9"

pattern Runtime_UNSUPPORTED :: Runtime
pattern Runtime_UNSUPPORTED = Runtime' "UNSUPPORTED"

{-# COMPLETE
  Runtime_GO_1_X,
  Runtime_JAVA_11,
  Runtime_JAVA_17,
  Runtime_JAVA_8,
  Runtime_JAVA_8_AL2,
  Runtime_NODEJS,
  Runtime_NODEJS_12_X,
  Runtime_NODEJS_14_X,
  Runtime_NODEJS_16_X,
  Runtime_NODEJS_18_X,
  Runtime_PYTHON_3_10,
  Runtime_PYTHON_3_7,
  Runtime_PYTHON_3_8,
  Runtime_PYTHON_3_9,
  Runtime_UNSUPPORTED,
  Runtime'
  #-}
