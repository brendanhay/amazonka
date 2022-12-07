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
-- Module      : Amazonka.CodeBuild.Types.EnvironmentVariableType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.EnvironmentVariableType
  ( EnvironmentVariableType
      ( ..,
        EnvironmentVariableType_PARAMETER_STORE,
        EnvironmentVariableType_PLAINTEXT,
        EnvironmentVariableType_SECRETS_MANAGER
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EnvironmentVariableType = EnvironmentVariableType'
  { fromEnvironmentVariableType ::
      Data.Text
  }
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

pattern EnvironmentVariableType_PARAMETER_STORE :: EnvironmentVariableType
pattern EnvironmentVariableType_PARAMETER_STORE = EnvironmentVariableType' "PARAMETER_STORE"

pattern EnvironmentVariableType_PLAINTEXT :: EnvironmentVariableType
pattern EnvironmentVariableType_PLAINTEXT = EnvironmentVariableType' "PLAINTEXT"

pattern EnvironmentVariableType_SECRETS_MANAGER :: EnvironmentVariableType
pattern EnvironmentVariableType_SECRETS_MANAGER = EnvironmentVariableType' "SECRETS_MANAGER"

{-# COMPLETE
  EnvironmentVariableType_PARAMETER_STORE,
  EnvironmentVariableType_PLAINTEXT,
  EnvironmentVariableType_SECRETS_MANAGER,
  EnvironmentVariableType'
  #-}
