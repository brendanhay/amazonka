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
-- Module      : Amazonka.Transfer.Types.CertificateStatusType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.CertificateStatusType
  ( CertificateStatusType
      ( ..,
        CertificateStatusType_ACTIVE,
        CertificateStatusType_INACTIVE,
        CertificateStatusType_PENDING_ROTATION
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype CertificateStatusType = CertificateStatusType'
  { fromCertificateStatusType ::
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

pattern CertificateStatusType_ACTIVE :: CertificateStatusType
pattern CertificateStatusType_ACTIVE = CertificateStatusType' "ACTIVE"

pattern CertificateStatusType_INACTIVE :: CertificateStatusType
pattern CertificateStatusType_INACTIVE = CertificateStatusType' "INACTIVE"

pattern CertificateStatusType_PENDING_ROTATION :: CertificateStatusType
pattern CertificateStatusType_PENDING_ROTATION = CertificateStatusType' "PENDING_ROTATION"

{-# COMPLETE
  CertificateStatusType_ACTIVE,
  CertificateStatusType_INACTIVE,
  CertificateStatusType_PENDING_ROTATION,
  CertificateStatusType'
  #-}
