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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CertificateDomainValidationStatus = CertificateDomainValidationStatus'
  { fromCertificateDomainValidationStatus ::
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
