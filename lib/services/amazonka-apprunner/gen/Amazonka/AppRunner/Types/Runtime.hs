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
-- Module      : Amazonka.AppRunner.Types.Runtime
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppRunner.Types.Runtime
  ( Runtime
      ( ..,
        Runtime_CORRETTO_11,
        Runtime_CORRETTO_8,
        Runtime_DOTNET_6,
        Runtime_GO_1,
        Runtime_NODEJS_12,
        Runtime_NODEJS_14,
        Runtime_NODEJS_16,
        Runtime_PHP_81,
        Runtime_PYTHON_3,
        Runtime_RUBY_31
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

pattern Runtime_CORRETTO_11 :: Runtime
pattern Runtime_CORRETTO_11 = Runtime' "CORRETTO_11"

pattern Runtime_CORRETTO_8 :: Runtime
pattern Runtime_CORRETTO_8 = Runtime' "CORRETTO_8"

pattern Runtime_DOTNET_6 :: Runtime
pattern Runtime_DOTNET_6 = Runtime' "DOTNET_6"

pattern Runtime_GO_1 :: Runtime
pattern Runtime_GO_1 = Runtime' "GO_1"

pattern Runtime_NODEJS_12 :: Runtime
pattern Runtime_NODEJS_12 = Runtime' "NODEJS_12"

pattern Runtime_NODEJS_14 :: Runtime
pattern Runtime_NODEJS_14 = Runtime' "NODEJS_14"

pattern Runtime_NODEJS_16 :: Runtime
pattern Runtime_NODEJS_16 = Runtime' "NODEJS_16"

pattern Runtime_PHP_81 :: Runtime
pattern Runtime_PHP_81 = Runtime' "PHP_81"

pattern Runtime_PYTHON_3 :: Runtime
pattern Runtime_PYTHON_3 = Runtime' "PYTHON_3"

pattern Runtime_RUBY_31 :: Runtime
pattern Runtime_RUBY_31 = Runtime' "RUBY_31"

{-# COMPLETE
  Runtime_CORRETTO_11,
  Runtime_CORRETTO_8,
  Runtime_DOTNET_6,
  Runtime_GO_1,
  Runtime_NODEJS_12,
  Runtime_NODEJS_14,
  Runtime_NODEJS_16,
  Runtime_PHP_81,
  Runtime_PYTHON_3,
  Runtime_RUBY_31,
  Runtime'
  #-}
