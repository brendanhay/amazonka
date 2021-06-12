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
-- Module      : Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateStatus
  ( LoadBalancerTlsCertificateStatus
      ( ..,
        LoadBalancerTlsCertificateStatus_EXPIRED,
        LoadBalancerTlsCertificateStatus_FAILED,
        LoadBalancerTlsCertificateStatus_INACTIVE,
        LoadBalancerTlsCertificateStatus_ISSUED,
        LoadBalancerTlsCertificateStatus_PENDING_VALIDATION,
        LoadBalancerTlsCertificateStatus_REVOKED,
        LoadBalancerTlsCertificateStatus_UNKNOWN,
        LoadBalancerTlsCertificateStatus_VALIDATION_TIMED_OUT
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype LoadBalancerTlsCertificateStatus = LoadBalancerTlsCertificateStatus'
  { fromLoadBalancerTlsCertificateStatus ::
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

pattern LoadBalancerTlsCertificateStatus_EXPIRED :: LoadBalancerTlsCertificateStatus
pattern LoadBalancerTlsCertificateStatus_EXPIRED = LoadBalancerTlsCertificateStatus' "EXPIRED"

pattern LoadBalancerTlsCertificateStatus_FAILED :: LoadBalancerTlsCertificateStatus
pattern LoadBalancerTlsCertificateStatus_FAILED = LoadBalancerTlsCertificateStatus' "FAILED"

pattern LoadBalancerTlsCertificateStatus_INACTIVE :: LoadBalancerTlsCertificateStatus
pattern LoadBalancerTlsCertificateStatus_INACTIVE = LoadBalancerTlsCertificateStatus' "INACTIVE"

pattern LoadBalancerTlsCertificateStatus_ISSUED :: LoadBalancerTlsCertificateStatus
pattern LoadBalancerTlsCertificateStatus_ISSUED = LoadBalancerTlsCertificateStatus' "ISSUED"

pattern LoadBalancerTlsCertificateStatus_PENDING_VALIDATION :: LoadBalancerTlsCertificateStatus
pattern LoadBalancerTlsCertificateStatus_PENDING_VALIDATION = LoadBalancerTlsCertificateStatus' "PENDING_VALIDATION"

pattern LoadBalancerTlsCertificateStatus_REVOKED :: LoadBalancerTlsCertificateStatus
pattern LoadBalancerTlsCertificateStatus_REVOKED = LoadBalancerTlsCertificateStatus' "REVOKED"

pattern LoadBalancerTlsCertificateStatus_UNKNOWN :: LoadBalancerTlsCertificateStatus
pattern LoadBalancerTlsCertificateStatus_UNKNOWN = LoadBalancerTlsCertificateStatus' "UNKNOWN"

pattern LoadBalancerTlsCertificateStatus_VALIDATION_TIMED_OUT :: LoadBalancerTlsCertificateStatus
pattern LoadBalancerTlsCertificateStatus_VALIDATION_TIMED_OUT = LoadBalancerTlsCertificateStatus' "VALIDATION_TIMED_OUT"

{-# COMPLETE
  LoadBalancerTlsCertificateStatus_EXPIRED,
  LoadBalancerTlsCertificateStatus_FAILED,
  LoadBalancerTlsCertificateStatus_INACTIVE,
  LoadBalancerTlsCertificateStatus_ISSUED,
  LoadBalancerTlsCertificateStatus_PENDING_VALIDATION,
  LoadBalancerTlsCertificateStatus_REVOKED,
  LoadBalancerTlsCertificateStatus_UNKNOWN,
  LoadBalancerTlsCertificateStatus_VALIDATION_TIMED_OUT,
  LoadBalancerTlsCertificateStatus'
  #-}
