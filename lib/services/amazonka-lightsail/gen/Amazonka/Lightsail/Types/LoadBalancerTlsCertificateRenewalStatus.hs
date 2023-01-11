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
-- Module      : Amazonka.Lightsail.Types.LoadBalancerTlsCertificateRenewalStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.LoadBalancerTlsCertificateRenewalStatus
  ( LoadBalancerTlsCertificateRenewalStatus
      ( ..,
        LoadBalancerTlsCertificateRenewalStatus_FAILED,
        LoadBalancerTlsCertificateRenewalStatus_PENDING_AUTO_RENEWAL,
        LoadBalancerTlsCertificateRenewalStatus_PENDING_VALIDATION,
        LoadBalancerTlsCertificateRenewalStatus_SUCCESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LoadBalancerTlsCertificateRenewalStatus = LoadBalancerTlsCertificateRenewalStatus'
  { fromLoadBalancerTlsCertificateRenewalStatus ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
