{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.InsufficientDataHealthStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.InsufficientDataHealthStatus
  ( InsufficientDataHealthStatus
      ( ..,
        InsufficientDataHealthStatus_Healthy,
        InsufficientDataHealthStatus_LastKnownStatus,
        InsufficientDataHealthStatus_Unhealthy
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Route53.Internal

newtype InsufficientDataHealthStatus = InsufficientDataHealthStatus'
  { fromInsufficientDataHealthStatus ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern InsufficientDataHealthStatus_Healthy :: InsufficientDataHealthStatus
pattern InsufficientDataHealthStatus_Healthy = InsufficientDataHealthStatus' "Healthy"

pattern InsufficientDataHealthStatus_LastKnownStatus :: InsufficientDataHealthStatus
pattern InsufficientDataHealthStatus_LastKnownStatus = InsufficientDataHealthStatus' "LastKnownStatus"

pattern InsufficientDataHealthStatus_Unhealthy :: InsufficientDataHealthStatus
pattern InsufficientDataHealthStatus_Unhealthy = InsufficientDataHealthStatus' "Unhealthy"

{-# COMPLETE
  InsufficientDataHealthStatus_Healthy,
  InsufficientDataHealthStatus_LastKnownStatus,
  InsufficientDataHealthStatus_Unhealthy,
  InsufficientDataHealthStatus'
  #-}
