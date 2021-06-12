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
-- Module      : Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateRenewalStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateRenewalStatus
  ( LoadBalancerTlsCertificateRenewalStatus
      ( ..,
        LoadBalancerTlsCertificateRenewalStatus_FAILED,
        LoadBalancerTlsCertificateRenewalStatus_PENDING_AUTO_RENEWAL,
        LoadBalancerTlsCertificateRenewalStatus_PENDING_VALIDATION,
        LoadBalancerTlsCertificateRenewalStatus_SUCCESS
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype LoadBalancerTlsCertificateRenewalStatus = LoadBalancerTlsCertificateRenewalStatus'
  { fromLoadBalancerTlsCertificateRenewalStatus ::
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

pattern LoadBalancerTlsCertificateRenewalStatus_FAILED :: LoadBalancerTlsCertificateRenewalStatus
pattern LoadBalancerTlsCertificateRenewalStatus_FAILED = LoadBalancerTlsCertificateRenewalStatus' "FAILED"

pattern LoadBalancerTlsCertificateRenewalStatus_PENDING_AUTO_RENEWAL :: LoadBalancerTlsCertificateRenewalStatus
pattern LoadBalancerTlsCertificateRenewalStatus_PENDING_AUTO_RENEWAL = LoadBalancerTlsCertificateRenewalStatus' "PENDING_AUTO_RENEWAL"

pattern LoadBalancerTlsCertificateRenewalStatus_PENDING_VALIDATION :: LoadBalancerTlsCertificateRenewalStatus
pattern LoadBalancerTlsCertificateRenewalStatus_PENDING_VALIDATION = LoadBalancerTlsCertificateRenewalStatus' "PENDING_VALIDATION"

pattern LoadBalancerTlsCertificateRenewalStatus_SUCCESS :: LoadBalancerTlsCertificateRenewalStatus
pattern LoadBalancerTlsCertificateRenewalStatus_SUCCESS = LoadBalancerTlsCertificateRenewalStatus' "SUCCESS"

{-# COMPLETE
  LoadBalancerTlsCertificateRenewalStatus_FAILED,
  LoadBalancerTlsCertificateRenewalStatus_PENDING_AUTO_RENEWAL,
  LoadBalancerTlsCertificateRenewalStatus_PENDING_VALIDATION,
  LoadBalancerTlsCertificateRenewalStatus_SUCCESS,
  LoadBalancerTlsCertificateRenewalStatus'
  #-}
