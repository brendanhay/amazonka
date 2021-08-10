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
-- Module      : Network.AWS.IoT.Types.CertificateStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.CertificateStatus
  ( CertificateStatus
      ( ..,
        CertificateStatus_ACTIVE,
        CertificateStatus_INACTIVE,
        CertificateStatus_PENDING_ACTIVATION,
        CertificateStatus_PENDING_TRANSFER,
        CertificateStatus_REGISTER_INACTIVE,
        CertificateStatus_REVOKED
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype CertificateStatus = CertificateStatus'
  { fromCertificateStatus ::
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

pattern CertificateStatus_ACTIVE :: CertificateStatus
pattern CertificateStatus_ACTIVE = CertificateStatus' "ACTIVE"

pattern CertificateStatus_INACTIVE :: CertificateStatus
pattern CertificateStatus_INACTIVE = CertificateStatus' "INACTIVE"

pattern CertificateStatus_PENDING_ACTIVATION :: CertificateStatus
pattern CertificateStatus_PENDING_ACTIVATION = CertificateStatus' "PENDING_ACTIVATION"

pattern CertificateStatus_PENDING_TRANSFER :: CertificateStatus
pattern CertificateStatus_PENDING_TRANSFER = CertificateStatus' "PENDING_TRANSFER"

pattern CertificateStatus_REGISTER_INACTIVE :: CertificateStatus
pattern CertificateStatus_REGISTER_INACTIVE = CertificateStatus' "REGISTER_INACTIVE"

pattern CertificateStatus_REVOKED :: CertificateStatus
pattern CertificateStatus_REVOKED = CertificateStatus' "REVOKED"

{-# COMPLETE
  CertificateStatus_ACTIVE,
  CertificateStatus_INACTIVE,
  CertificateStatus_PENDING_ACTIVATION,
  CertificateStatus_PENDING_TRANSFER,
  CertificateStatus_REGISTER_INACTIVE,
  CertificateStatus_REVOKED,
  CertificateStatus'
  #-}
