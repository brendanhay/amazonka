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
-- Module      : Network.AWS.ElastiCache.Types.ServiceUpdateStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.ServiceUpdateStatus
  ( ServiceUpdateStatus
      ( ..,
        ServiceUpdateStatus_Available,
        ServiceUpdateStatus_Cancelled,
        ServiceUpdateStatus_Expired
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ServiceUpdateStatus = ServiceUpdateStatus'
  { fromServiceUpdateStatus ::
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

pattern ServiceUpdateStatus_Available :: ServiceUpdateStatus
pattern ServiceUpdateStatus_Available = ServiceUpdateStatus' "available"

pattern ServiceUpdateStatus_Cancelled :: ServiceUpdateStatus
pattern ServiceUpdateStatus_Cancelled = ServiceUpdateStatus' "cancelled"

pattern ServiceUpdateStatus_Expired :: ServiceUpdateStatus
pattern ServiceUpdateStatus_Expired = ServiceUpdateStatus' "expired"

{-# COMPLETE
  ServiceUpdateStatus_Available,
  ServiceUpdateStatus_Cancelled,
  ServiceUpdateStatus_Expired,
  ServiceUpdateStatus'
  #-}
