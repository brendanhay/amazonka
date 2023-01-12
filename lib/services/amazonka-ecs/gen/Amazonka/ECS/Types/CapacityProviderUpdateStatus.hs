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
-- Module      : Amazonka.ECS.Types.CapacityProviderUpdateStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.CapacityProviderUpdateStatus
  ( CapacityProviderUpdateStatus
      ( ..,
        CapacityProviderUpdateStatus_DELETE_COMPLETE,
        CapacityProviderUpdateStatus_DELETE_FAILED,
        CapacityProviderUpdateStatus_DELETE_IN_PROGRESS,
        CapacityProviderUpdateStatus_UPDATE_COMPLETE,
        CapacityProviderUpdateStatus_UPDATE_FAILED,
        CapacityProviderUpdateStatus_UPDATE_IN_PROGRESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CapacityProviderUpdateStatus = CapacityProviderUpdateStatus'
  { fromCapacityProviderUpdateStatus ::
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

pattern CapacityProviderUpdateStatus_DELETE_COMPLETE :: CapacityProviderUpdateStatus
pattern CapacityProviderUpdateStatus_DELETE_COMPLETE = CapacityProviderUpdateStatus' "DELETE_COMPLETE"

pattern CapacityProviderUpdateStatus_DELETE_FAILED :: CapacityProviderUpdateStatus
pattern CapacityProviderUpdateStatus_DELETE_FAILED = CapacityProviderUpdateStatus' "DELETE_FAILED"

pattern CapacityProviderUpdateStatus_DELETE_IN_PROGRESS :: CapacityProviderUpdateStatus
pattern CapacityProviderUpdateStatus_DELETE_IN_PROGRESS = CapacityProviderUpdateStatus' "DELETE_IN_PROGRESS"

pattern CapacityProviderUpdateStatus_UPDATE_COMPLETE :: CapacityProviderUpdateStatus
pattern CapacityProviderUpdateStatus_UPDATE_COMPLETE = CapacityProviderUpdateStatus' "UPDATE_COMPLETE"

pattern CapacityProviderUpdateStatus_UPDATE_FAILED :: CapacityProviderUpdateStatus
pattern CapacityProviderUpdateStatus_UPDATE_FAILED = CapacityProviderUpdateStatus' "UPDATE_FAILED"

pattern CapacityProviderUpdateStatus_UPDATE_IN_PROGRESS :: CapacityProviderUpdateStatus
pattern CapacityProviderUpdateStatus_UPDATE_IN_PROGRESS = CapacityProviderUpdateStatus' "UPDATE_IN_PROGRESS"

{-# COMPLETE
  CapacityProviderUpdateStatus_DELETE_COMPLETE,
  CapacityProviderUpdateStatus_DELETE_FAILED,
  CapacityProviderUpdateStatus_DELETE_IN_PROGRESS,
  CapacityProviderUpdateStatus_UPDATE_COMPLETE,
  CapacityProviderUpdateStatus_UPDATE_FAILED,
  CapacityProviderUpdateStatus_UPDATE_IN_PROGRESS,
  CapacityProviderUpdateStatus'
  #-}
