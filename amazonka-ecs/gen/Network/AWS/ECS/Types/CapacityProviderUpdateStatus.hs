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
-- Module      : Network.AWS.ECS.Types.CapacityProviderUpdateStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.CapacityProviderUpdateStatus
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype CapacityProviderUpdateStatus = CapacityProviderUpdateStatus'
  { fromCapacityProviderUpdateStatus ::
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
