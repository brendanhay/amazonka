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
-- Module      : Network.AWS.ECS.Types.CapacityProviderStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.CapacityProviderStatus
  ( CapacityProviderStatus
      ( ..,
        CapacityProviderStatus_ACTIVE,
        CapacityProviderStatus_INACTIVE
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype CapacityProviderStatus = CapacityProviderStatus'
  { fromCapacityProviderStatus ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern CapacityProviderStatus_ACTIVE :: CapacityProviderStatus
pattern CapacityProviderStatus_ACTIVE = CapacityProviderStatus' "ACTIVE"

pattern CapacityProviderStatus_INACTIVE :: CapacityProviderStatus
pattern CapacityProviderStatus_INACTIVE = CapacityProviderStatus' "INACTIVE"

{-# COMPLETE
  CapacityProviderStatus_ACTIVE,
  CapacityProviderStatus_INACTIVE,
  CapacityProviderStatus'
  #-}
