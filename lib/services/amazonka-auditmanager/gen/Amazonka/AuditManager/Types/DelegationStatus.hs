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
-- Module      : Amazonka.AuditManager.Types.DelegationStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.DelegationStatus
  ( DelegationStatus
      ( ..,
        DelegationStatus_COMPLETE,
        DelegationStatus_IN_PROGRESS,
        DelegationStatus_UNDER_REVIEW
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DelegationStatus = DelegationStatus'
  { fromDelegationStatus ::
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
