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
-- Module      : Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateDomainStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateDomainStatus
  ( LoadBalancerTlsCertificateDomainStatus
      ( ..,
        LoadBalancerTlsCertificateDomainStatus_FAILED,
        LoadBalancerTlsCertificateDomainStatus_PENDING_VALIDATION,
        LoadBalancerTlsCertificateDomainStatus_SUCCESS
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype LoadBalancerTlsCertificateDomainStatus = LoadBalancerTlsCertificateDomainStatus'
  { fromLoadBalancerTlsCertificateDomainStatus ::
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

pattern LoadBalancerTlsCertificateDomainStatus_FAILED :: LoadBalancerTlsCertificateDomainStatus
pattern LoadBalancerTlsCertificateDomainStatus_FAILED = LoadBalancerTlsCertificateDomainStatus' "FAILED"

pattern LoadBalancerTlsCertificateDomainStatus_PENDING_VALIDATION :: LoadBalancerTlsCertificateDomainStatus
pattern LoadBalancerTlsCertificateDomainStatus_PENDING_VALIDATION = LoadBalancerTlsCertificateDomainStatus' "PENDING_VALIDATION"

pattern LoadBalancerTlsCertificateDomainStatus_SUCCESS :: LoadBalancerTlsCertificateDomainStatus
pattern LoadBalancerTlsCertificateDomainStatus_SUCCESS = LoadBalancerTlsCertificateDomainStatus' "SUCCESS"

{-# COMPLETE
  LoadBalancerTlsCertificateDomainStatus_FAILED,
  LoadBalancerTlsCertificateDomainStatus_PENDING_VALIDATION,
  LoadBalancerTlsCertificateDomainStatus_SUCCESS,
  LoadBalancerTlsCertificateDomainStatus'
  #-}
