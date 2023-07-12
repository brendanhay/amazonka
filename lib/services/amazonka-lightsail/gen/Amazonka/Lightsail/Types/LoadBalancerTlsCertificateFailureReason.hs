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
-- Module      : Amazonka.Lightsail.Types.LoadBalancerTlsCertificateFailureReason
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.LoadBalancerTlsCertificateFailureReason
  ( LoadBalancerTlsCertificateFailureReason
      ( ..,
        LoadBalancerTlsCertificateFailureReason_ADDITIONAL_VERIFICATION_REQUIRED,
        LoadBalancerTlsCertificateFailureReason_DOMAIN_NOT_ALLOWED,
        LoadBalancerTlsCertificateFailureReason_INVALID_PUBLIC_DOMAIN,
        LoadBalancerTlsCertificateFailureReason_NO_AVAILABLE_CONTACTS,
        LoadBalancerTlsCertificateFailureReason_OTHER
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LoadBalancerTlsCertificateFailureReason = LoadBalancerTlsCertificateFailureReason'
  { fromLoadBalancerTlsCertificateFailureReason ::
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

pattern LoadBalancerTlsCertificateFailureReason_ADDITIONAL_VERIFICATION_REQUIRED :: LoadBalancerTlsCertificateFailureReason
pattern LoadBalancerTlsCertificateFailureReason_ADDITIONAL_VERIFICATION_REQUIRED = LoadBalancerTlsCertificateFailureReason' "ADDITIONAL_VERIFICATION_REQUIRED"

pattern LoadBalancerTlsCertificateFailureReason_DOMAIN_NOT_ALLOWED :: LoadBalancerTlsCertificateFailureReason
pattern LoadBalancerTlsCertificateFailureReason_DOMAIN_NOT_ALLOWED = LoadBalancerTlsCertificateFailureReason' "DOMAIN_NOT_ALLOWED"

pattern LoadBalancerTlsCertificateFailureReason_INVALID_PUBLIC_DOMAIN :: LoadBalancerTlsCertificateFailureReason
pattern LoadBalancerTlsCertificateFailureReason_INVALID_PUBLIC_DOMAIN = LoadBalancerTlsCertificateFailureReason' "INVALID_PUBLIC_DOMAIN"

pattern LoadBalancerTlsCertificateFailureReason_NO_AVAILABLE_CONTACTS :: LoadBalancerTlsCertificateFailureReason
pattern LoadBalancerTlsCertificateFailureReason_NO_AVAILABLE_CONTACTS = LoadBalancerTlsCertificateFailureReason' "NO_AVAILABLE_CONTACTS"

pattern LoadBalancerTlsCertificateFailureReason_OTHER :: LoadBalancerTlsCertificateFailureReason
pattern LoadBalancerTlsCertificateFailureReason_OTHER = LoadBalancerTlsCertificateFailureReason' "OTHER"

{-# COMPLETE
  LoadBalancerTlsCertificateFailureReason_ADDITIONAL_VERIFICATION_REQUIRED,
  LoadBalancerTlsCertificateFailureReason_DOMAIN_NOT_ALLOWED,
  LoadBalancerTlsCertificateFailureReason_INVALID_PUBLIC_DOMAIN,
  LoadBalancerTlsCertificateFailureReason_NO_AVAILABLE_CONTACTS,
  LoadBalancerTlsCertificateFailureReason_OTHER,
  LoadBalancerTlsCertificateFailureReason'
  #-}
