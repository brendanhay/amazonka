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
-- Module      : Amazonka.SSMIncidents.Types.SsmTargetAccount
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMIncidents.Types.SsmTargetAccount
  ( SsmTargetAccount
      ( ..,
        SsmTargetAccount_IMPACTED_ACCOUNT,
        SsmTargetAccount_RESPONSE_PLAN_OWNER_ACCOUNT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SsmTargetAccount = SsmTargetAccount'
  { fromSsmTargetAccount ::
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

pattern SsmTargetAccount_IMPACTED_ACCOUNT :: SsmTargetAccount
pattern SsmTargetAccount_IMPACTED_ACCOUNT = SsmTargetAccount' "IMPACTED_ACCOUNT"

pattern SsmTargetAccount_RESPONSE_PLAN_OWNER_ACCOUNT :: SsmTargetAccount
pattern SsmTargetAccount_RESPONSE_PLAN_OWNER_ACCOUNT = SsmTargetAccount' "RESPONSE_PLAN_OWNER_ACCOUNT"

{-# COMPLETE
  SsmTargetAccount_IMPACTED_ACCOUNT,
  SsmTargetAccount_RESPONSE_PLAN_OWNER_ACCOUNT,
  SsmTargetAccount'
  #-}
