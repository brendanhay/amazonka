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
-- Module      : Amazonka.Lightsail.Types.LoadBalancerTlsCertificateRevocationReason
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.LoadBalancerTlsCertificateRevocationReason
  ( LoadBalancerTlsCertificateRevocationReason
      ( ..,
        LoadBalancerTlsCertificateRevocationReason_AFFILIATION_CHANGED,
        LoadBalancerTlsCertificateRevocationReason_A_A_COMPROMISE,
        LoadBalancerTlsCertificateRevocationReason_CA_COMPROMISE,
        LoadBalancerTlsCertificateRevocationReason_CERTIFICATE_HOLD,
        LoadBalancerTlsCertificateRevocationReason_CESSATION_OF_OPERATION,
        LoadBalancerTlsCertificateRevocationReason_KEY_COMPROMISE,
        LoadBalancerTlsCertificateRevocationReason_PRIVILEGE_WITHDRAWN,
        LoadBalancerTlsCertificateRevocationReason_REMOVE_FROM_CRL,
        LoadBalancerTlsCertificateRevocationReason_SUPERCEDED,
        LoadBalancerTlsCertificateRevocationReason_UNSPECIFIED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LoadBalancerTlsCertificateRevocationReason = LoadBalancerTlsCertificateRevocationReason'
  { fromLoadBalancerTlsCertificateRevocationReason ::
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

pattern LoadBalancerTlsCertificateRevocationReason_AFFILIATION_CHANGED :: LoadBalancerTlsCertificateRevocationReason
pattern LoadBalancerTlsCertificateRevocationReason_AFFILIATION_CHANGED = LoadBalancerTlsCertificateRevocationReason' "AFFILIATION_CHANGED"

pattern LoadBalancerTlsCertificateRevocationReason_A_A_COMPROMISE :: LoadBalancerTlsCertificateRevocationReason
pattern LoadBalancerTlsCertificateRevocationReason_A_A_COMPROMISE = LoadBalancerTlsCertificateRevocationReason' "A_A_COMPROMISE"

pattern LoadBalancerTlsCertificateRevocationReason_CA_COMPROMISE :: LoadBalancerTlsCertificateRevocationReason
pattern LoadBalancerTlsCertificateRevocationReason_CA_COMPROMISE = LoadBalancerTlsCertificateRevocationReason' "CA_COMPROMISE"

pattern LoadBalancerTlsCertificateRevocationReason_CERTIFICATE_HOLD :: LoadBalancerTlsCertificateRevocationReason
pattern LoadBalancerTlsCertificateRevocationReason_CERTIFICATE_HOLD = LoadBalancerTlsCertificateRevocationReason' "CERTIFICATE_HOLD"

pattern LoadBalancerTlsCertificateRevocationReason_CESSATION_OF_OPERATION :: LoadBalancerTlsCertificateRevocationReason
pattern LoadBalancerTlsCertificateRevocationReason_CESSATION_OF_OPERATION = LoadBalancerTlsCertificateRevocationReason' "CESSATION_OF_OPERATION"

pattern LoadBalancerTlsCertificateRevocationReason_KEY_COMPROMISE :: LoadBalancerTlsCertificateRevocationReason
pattern LoadBalancerTlsCertificateRevocationReason_KEY_COMPROMISE = LoadBalancerTlsCertificateRevocationReason' "KEY_COMPROMISE"

pattern LoadBalancerTlsCertificateRevocationReason_PRIVILEGE_WITHDRAWN :: LoadBalancerTlsCertificateRevocationReason
pattern LoadBalancerTlsCertificateRevocationReason_PRIVILEGE_WITHDRAWN = LoadBalancerTlsCertificateRevocationReason' "PRIVILEGE_WITHDRAWN"

pattern LoadBalancerTlsCertificateRevocationReason_REMOVE_FROM_CRL :: LoadBalancerTlsCertificateRevocationReason
pattern LoadBalancerTlsCertificateRevocationReason_REMOVE_FROM_CRL = LoadBalancerTlsCertificateRevocationReason' "REMOVE_FROM_CRL"

pattern LoadBalancerTlsCertificateRevocationReason_SUPERCEDED :: LoadBalancerTlsCertificateRevocationReason
pattern LoadBalancerTlsCertificateRevocationReason_SUPERCEDED = LoadBalancerTlsCertificateRevocationReason' "SUPERCEDED"

pattern LoadBalancerTlsCertificateRevocationReason_UNSPECIFIED :: LoadBalancerTlsCertificateRevocationReason
pattern LoadBalancerTlsCertificateRevocationReason_UNSPECIFIED = LoadBalancerTlsCertificateRevocationReason' "UNSPECIFIED"

{-# COMPLETE
  LoadBalancerTlsCertificateRevocationReason_AFFILIATION_CHANGED,
  LoadBalancerTlsCertificateRevocationReason_A_A_COMPROMISE,
  LoadBalancerTlsCertificateRevocationReason_CA_COMPROMISE,
  LoadBalancerTlsCertificateRevocationReason_CERTIFICATE_HOLD,
  LoadBalancerTlsCertificateRevocationReason_CESSATION_OF_OPERATION,
  LoadBalancerTlsCertificateRevocationReason_KEY_COMPROMISE,
  LoadBalancerTlsCertificateRevocationReason_PRIVILEGE_WITHDRAWN,
  LoadBalancerTlsCertificateRevocationReason_REMOVE_FROM_CRL,
  LoadBalancerTlsCertificateRevocationReason_SUPERCEDED,
  LoadBalancerTlsCertificateRevocationReason_UNSPECIFIED,
  LoadBalancerTlsCertificateRevocationReason'
  #-}
