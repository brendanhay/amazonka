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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CreateAccountState = CreateAccountState'
  { fromCreateAccountState ::
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
