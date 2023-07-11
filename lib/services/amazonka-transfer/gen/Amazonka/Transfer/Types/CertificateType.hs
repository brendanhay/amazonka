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
-- Module      : Amazonka.Transfer.Types.CertificateType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.CertificateType
  ( CertificateType
      ( ..,
        CertificateType_CERTIFICATE,
        CertificateType_CERTIFICATE_WITH_PRIVATE_KEY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CertificateType = CertificateType'
  { fromCertificateType ::
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

pattern CertificateType_CERTIFICATE :: CertificateType
pattern CertificateType_CERTIFICATE = CertificateType' "CERTIFICATE"

pattern CertificateType_CERTIFICATE_WITH_PRIVATE_KEY :: CertificateType
pattern CertificateType_CERTIFICATE_WITH_PRIVATE_KEY = CertificateType' "CERTIFICATE_WITH_PRIVATE_KEY"

{-# COMPLETE
  CertificateType_CERTIFICATE,
  CertificateType_CERTIFICATE_WITH_PRIVATE_KEY,
  CertificateType'
  #-}
