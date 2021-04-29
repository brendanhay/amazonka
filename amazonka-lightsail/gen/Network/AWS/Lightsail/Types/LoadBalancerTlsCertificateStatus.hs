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

import qualified Network.AWS.Prelude as Prelude

newtype LoadBalancerTlsCertificateStatus = LoadBalancerTlsCertificateStatus'
  { fromLoadBalancerTlsCertificateStatus ::
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
