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
-- Module      : Amazonka.Lightsail.Types.LoadBalancerTlsCertificateDomainStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.LoadBalancerTlsCertificateDomainStatus
  ( LoadBalancerTlsCertificateDomainStatus
      ( ..,
        LoadBalancerTlsCertificateDomainStatus_FAILED,
        LoadBalancerTlsCertificateDomainStatus_PENDING_VALIDATION,
        LoadBalancerTlsCertificateDomainStatus_SUCCESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LoadBalancerTlsCertificateDomainStatus = LoadBalancerTlsCertificateDomainStatus'
  { fromLoadBalancerTlsCertificateDomainStatus ::
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
