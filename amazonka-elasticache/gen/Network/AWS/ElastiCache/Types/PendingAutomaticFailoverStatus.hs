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
-- Module      : Network.AWS.ElastiCache.Types.PendingAutomaticFailoverStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.PendingAutomaticFailoverStatus
  ( PendingAutomaticFailoverStatus
      ( ..,
        PendingAutomaticFailoverStatus_Disabled,
        PendingAutomaticFailoverStatus_Enabled
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype PendingAutomaticFailoverStatus = PendingAutomaticFailoverStatus'
  { fromPendingAutomaticFailoverStatus ::
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

pattern PendingAutomaticFailoverStatus_Disabled :: PendingAutomaticFailoverStatus
pattern PendingAutomaticFailoverStatus_Disabled = PendingAutomaticFailoverStatus' "disabled"

pattern PendingAutomaticFailoverStatus_Enabled :: PendingAutomaticFailoverStatus
pattern PendingAutomaticFailoverStatus_Enabled = PendingAutomaticFailoverStatus' "enabled"

{-# COMPLETE
  PendingAutomaticFailoverStatus_Disabled,
  PendingAutomaticFailoverStatus_Enabled,
  PendingAutomaticFailoverStatus'
  #-}
