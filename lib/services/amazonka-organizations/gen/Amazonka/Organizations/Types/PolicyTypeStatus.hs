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
-- Module      : Amazonka.Organizations.Types.PolicyTypeStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Organizations.Types.PolicyTypeStatus
  ( PolicyTypeStatus
      ( ..,
        PolicyTypeStatus_ENABLED,
        PolicyTypeStatus_PENDING_DISABLE,
        PolicyTypeStatus_PENDING_ENABLE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PolicyTypeStatus = PolicyTypeStatus'
  { fromPolicyTypeStatus ::
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

pattern PolicyTypeStatus_ENABLED :: PolicyTypeStatus
pattern PolicyTypeStatus_ENABLED = PolicyTypeStatus' "ENABLED"

pattern PolicyTypeStatus_PENDING_DISABLE :: PolicyTypeStatus
pattern PolicyTypeStatus_PENDING_DISABLE = PolicyTypeStatus' "PENDING_DISABLE"

pattern PolicyTypeStatus_PENDING_ENABLE :: PolicyTypeStatus
pattern PolicyTypeStatus_PENDING_ENABLE = PolicyTypeStatus' "PENDING_ENABLE"

{-# COMPLETE
  PolicyTypeStatus_ENABLED,
  PolicyTypeStatus_PENDING_DISABLE,
  PolicyTypeStatus_PENDING_ENABLE,
  PolicyTypeStatus'
  #-}
