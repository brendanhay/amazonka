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

import qualified Network.AWS.Prelude as Prelude

newtype LoadBalancerTlsCertificateDomainStatus = LoadBalancerTlsCertificateDomainStatus'
  { fromLoadBalancerTlsCertificateDomainStatus ::
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
