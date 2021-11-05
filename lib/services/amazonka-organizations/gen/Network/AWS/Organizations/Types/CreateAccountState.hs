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
-- Module      : Amazonka.Organizations.Types.CreateAccountState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Organizations.Types.CreateAccountState
  ( CreateAccountState
      ( ..,
        CreateAccountState_FAILED,
        CreateAccountState_IN_PROGRESS,
        CreateAccountState_SUCCEEDED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype CreateAccountState = CreateAccountState'
  { fromCreateAccountState ::
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

pattern CreateAccountState_FAILED :: CreateAccountState
pattern CreateAccountState_FAILED = CreateAccountState' "FAILED"

pattern CreateAccountState_IN_PROGRESS :: CreateAccountState
pattern CreateAccountState_IN_PROGRESS = CreateAccountState' "IN_PROGRESS"

pattern CreateAccountState_SUCCEEDED :: CreateAccountState
pattern CreateAccountState_SUCCEEDED = CreateAccountState' "SUCCEEDED"

{-# COMPLETE
  CreateAccountState_FAILED,
  CreateAccountState_IN_PROGRESS,
  CreateAccountState_SUCCEEDED,
  CreateAccountState'
  #-}
