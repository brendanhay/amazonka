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
-- Module      : Network.AWS.AuditManager.Types.DelegationStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AuditManager.Types.DelegationStatus
  ( DelegationStatus
      ( ..,
        DelegationStatus_COMPLETE,
        DelegationStatus_IN_PROGRESS,
        DelegationStatus_UNDER_REVIEW
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype DelegationStatus = DelegationStatus'
  { fromDelegationStatus ::
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

pattern DelegationStatus_COMPLETE :: DelegationStatus
pattern DelegationStatus_COMPLETE = DelegationStatus' "COMPLETE"

pattern DelegationStatus_IN_PROGRESS :: DelegationStatus
pattern DelegationStatus_IN_PROGRESS = DelegationStatus' "IN_PROGRESS"

pattern DelegationStatus_UNDER_REVIEW :: DelegationStatus
pattern DelegationStatus_UNDER_REVIEW = DelegationStatus' "UNDER_REVIEW"

{-# COMPLETE
  DelegationStatus_COMPLETE,
  DelegationStatus_IN_PROGRESS,
  DelegationStatus_UNDER_REVIEW,
  DelegationStatus'
  #-}
