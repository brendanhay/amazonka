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
-- Module      : Amazonka.Proton.Types.EnvironmentAccountConnectionRequesterAccountType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Proton.Types.EnvironmentAccountConnectionRequesterAccountType
  ( EnvironmentAccountConnectionRequesterAccountType
      ( ..,
        EnvironmentAccountConnectionRequesterAccountType_ENVIRONMENT_ACCOUNT,
        EnvironmentAccountConnectionRequesterAccountType_MANAGEMENT_ACCOUNT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype EnvironmentAccountConnectionRequesterAccountType = EnvironmentAccountConnectionRequesterAccountType'
  { fromEnvironmentAccountConnectionRequesterAccountType ::
      Core.Text
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

pattern EnvironmentAccountConnectionRequesterAccountType_ENVIRONMENT_ACCOUNT :: EnvironmentAccountConnectionRequesterAccountType
pattern EnvironmentAccountConnectionRequesterAccountType_ENVIRONMENT_ACCOUNT = EnvironmentAccountConnectionRequesterAccountType' "ENVIRONMENT_ACCOUNT"

pattern EnvironmentAccountConnectionRequesterAccountType_MANAGEMENT_ACCOUNT :: EnvironmentAccountConnectionRequesterAccountType
pattern EnvironmentAccountConnectionRequesterAccountType_MANAGEMENT_ACCOUNT = EnvironmentAccountConnectionRequesterAccountType' "MANAGEMENT_ACCOUNT"

{-# COMPLETE
  EnvironmentAccountConnectionRequesterAccountType_ENVIRONMENT_ACCOUNT,
  EnvironmentAccountConnectionRequesterAccountType_MANAGEMENT_ACCOUNT,
  EnvironmentAccountConnectionRequesterAccountType'
  #-}
