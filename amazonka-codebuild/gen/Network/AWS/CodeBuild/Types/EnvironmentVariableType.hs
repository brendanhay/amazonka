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
-- Module      : Network.AWS.CodeBuild.Types.EnvironmentVariableType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.EnvironmentVariableType
  ( EnvironmentVariableType
      ( ..,
        EnvironmentVariableType_PARAMETER_STORE,
        EnvironmentVariableType_PLAINTEXT,
        EnvironmentVariableType_SECRETS_MANAGER
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype EnvironmentVariableType = EnvironmentVariableType'
  { fromEnvironmentVariableType ::
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
