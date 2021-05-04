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
-- Module      : Network.AWS.Route53AutoNaming.Types.HealthCheckType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.HealthCheckType
  ( HealthCheckType
      ( ..,
        HealthCheckType_HTTP,
        HealthCheckType_HTTPS,
        HealthCheckType_TCP
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype HealthCheckType = HealthCheckType'
  { fromHealthCheckType ::
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

pattern HealthCheckType_HTTP :: HealthCheckType
pattern HealthCheckType_HTTP = HealthCheckType' "HTTP"

pattern HealthCheckType_HTTPS :: HealthCheckType
pattern HealthCheckType_HTTPS = HealthCheckType' "HTTPS"

pattern HealthCheckType_TCP :: HealthCheckType
pattern HealthCheckType_TCP = HealthCheckType' "TCP"

{-# COMPLETE
  HealthCheckType_HTTP,
  HealthCheckType_HTTPS,
  HealthCheckType_TCP,
  HealthCheckType'
  #-}
