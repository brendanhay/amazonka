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
-- Module      : Amazonka.Lightsail.Types.LoadBalancerTlsCertificateDnsRecordCreationStateCode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.LoadBalancerTlsCertificateDnsRecordCreationStateCode
  ( LoadBalancerTlsCertificateDnsRecordCreationStateCode
      ( ..,
        LoadBalancerTlsCertificateDnsRecordCreationStateCode_FAILED,
        LoadBalancerTlsCertificateDnsRecordCreationStateCode_STARTED,
        LoadBalancerTlsCertificateDnsRecordCreationStateCode_SUCCEEDED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype LoadBalancerTlsCertificateDnsRecordCreationStateCode = LoadBalancerTlsCertificateDnsRecordCreationStateCode'
  { fromLoadBalancerTlsCertificateDnsRecordCreationStateCode ::
      Core.Text
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

pattern LoadBalancerTlsCertificateDnsRecordCreationStateCode_FAILED :: LoadBalancerTlsCertificateDnsRecordCreationStateCode
pattern LoadBalancerTlsCertificateDnsRecordCreationStateCode_FAILED = LoadBalancerTlsCertificateDnsRecordCreationStateCode' "FAILED"

pattern LoadBalancerTlsCertificateDnsRecordCreationStateCode_STARTED :: LoadBalancerTlsCertificateDnsRecordCreationStateCode
pattern LoadBalancerTlsCertificateDnsRecordCreationStateCode_STARTED = LoadBalancerTlsCertificateDnsRecordCreationStateCode' "STARTED"

pattern LoadBalancerTlsCertificateDnsRecordCreationStateCode_SUCCEEDED :: LoadBalancerTlsCertificateDnsRecordCreationStateCode
pattern LoadBalancerTlsCertificateDnsRecordCreationStateCode_SUCCEEDED = LoadBalancerTlsCertificateDnsRecordCreationStateCode' "SUCCEEDED"

{-# COMPLETE
  LoadBalancerTlsCertificateDnsRecordCreationStateCode_FAILED,
  LoadBalancerTlsCertificateDnsRecordCreationStateCode_STARTED,
  LoadBalancerTlsCertificateDnsRecordCreationStateCode_SUCCEEDED,
  LoadBalancerTlsCertificateDnsRecordCreationStateCode'
  #-}
