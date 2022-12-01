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
-- Module      : Amazonka.Lightsail.Types.CertificateDomainValidationStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.CertificateDomainValidationStatus
  ( CertificateDomainValidationStatus
      ( ..,
        CertificateDomainValidationStatus_FAILED,
        CertificateDomainValidationStatus_PENDING_VALIDATION,
        CertificateDomainValidationStatus_SUCCESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype CertificateDomainValidationStatus = CertificateDomainValidationStatus'
  { fromCertificateDomainValidationStatus ::
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

pattern CertificateDomainValidationStatus_FAILED :: CertificateDomainValidationStatus
pattern CertificateDomainValidationStatus_FAILED = CertificateDomainValidationStatus' "FAILED"

pattern CertificateDomainValidationStatus_PENDING_VALIDATION :: CertificateDomainValidationStatus
pattern CertificateDomainValidationStatus_PENDING_VALIDATION = CertificateDomainValidationStatus' "PENDING_VALIDATION"

pattern CertificateDomainValidationStatus_SUCCESS :: CertificateDomainValidationStatus
pattern CertificateDomainValidationStatus_SUCCESS = CertificateDomainValidationStatus' "SUCCESS"

{-# COMPLETE
  CertificateDomainValidationStatus_FAILED,
  CertificateDomainValidationStatus_PENDING_VALIDATION,
  CertificateDomainValidationStatus_SUCCESS,
  CertificateDomainValidationStatus'
  #-}
