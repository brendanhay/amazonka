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
-- Module      : Amazonka.CertificateManagerPCA.Types.CertificateAuthorityStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CertificateManagerPCA.Types.CertificateAuthorityStatus
  ( CertificateAuthorityStatus
      ( ..,
        CertificateAuthorityStatus_ACTIVE,
        CertificateAuthorityStatus_CREATING,
        CertificateAuthorityStatus_DELETED,
        CertificateAuthorityStatus_DISABLED,
        CertificateAuthorityStatus_EXPIRED,
        CertificateAuthorityStatus_FAILED,
        CertificateAuthorityStatus_PENDING_CERTIFICATE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CertificateAuthorityStatus = CertificateAuthorityStatus'
  { fromCertificateAuthorityStatus ::
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

pattern CertificateAuthorityStatus_ACTIVE :: CertificateAuthorityStatus
pattern CertificateAuthorityStatus_ACTIVE = CertificateAuthorityStatus' "ACTIVE"

pattern CertificateAuthorityStatus_CREATING :: CertificateAuthorityStatus
pattern CertificateAuthorityStatus_CREATING = CertificateAuthorityStatus' "CREATING"

pattern CertificateAuthorityStatus_DELETED :: CertificateAuthorityStatus
pattern CertificateAuthorityStatus_DELETED = CertificateAuthorityStatus' "DELETED"

pattern CertificateAuthorityStatus_DISABLED :: CertificateAuthorityStatus
pattern CertificateAuthorityStatus_DISABLED = CertificateAuthorityStatus' "DISABLED"

pattern CertificateAuthorityStatus_EXPIRED :: CertificateAuthorityStatus
pattern CertificateAuthorityStatus_EXPIRED = CertificateAuthorityStatus' "EXPIRED"

pattern CertificateAuthorityStatus_FAILED :: CertificateAuthorityStatus
pattern CertificateAuthorityStatus_FAILED = CertificateAuthorityStatus' "FAILED"

pattern CertificateAuthorityStatus_PENDING_CERTIFICATE :: CertificateAuthorityStatus
pattern CertificateAuthorityStatus_PENDING_CERTIFICATE = CertificateAuthorityStatus' "PENDING_CERTIFICATE"

{-# COMPLETE
  CertificateAuthorityStatus_ACTIVE,
  CertificateAuthorityStatus_CREATING,
  CertificateAuthorityStatus_DELETED,
  CertificateAuthorityStatus_DISABLED,
  CertificateAuthorityStatus_EXPIRED,
  CertificateAuthorityStatus_FAILED,
  CertificateAuthorityStatus_PENDING_CERTIFICATE,
  CertificateAuthorityStatus'
  #-}
