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
-- Module      : Network.AWS.Organizations.Types.PolicyTypeStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.PolicyTypeStatus
  ( PolicyTypeStatus
      ( ..,
        PolicyTypeStatus_ENABLED,
        PolicyTypeStatus_PENDING_DISABLE,
        PolicyTypeStatus_PENDING_ENABLE
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype PolicyTypeStatus = PolicyTypeStatus'
  { fromPolicyTypeStatus ::
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
