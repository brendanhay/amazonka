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
-- Module      : Amazonka.CertificateManagerPCA.Types.CertificateAuthorityUsageMode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CertificateManagerPCA.Types.CertificateAuthorityUsageMode
  ( CertificateAuthorityUsageMode
      ( ..,
        CertificateAuthorityUsageMode_GENERAL_PURPOSE,
        CertificateAuthorityUsageMode_SHORT_LIVED_CERTIFICATE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CertificateAuthorityUsageMode = CertificateAuthorityUsageMode'
  { fromCertificateAuthorityUsageMode ::
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

pattern CertificateAuthorityUsageMode_GENERAL_PURPOSE :: CertificateAuthorityUsageMode
pattern CertificateAuthorityUsageMode_GENERAL_PURPOSE = CertificateAuthorityUsageMode' "GENERAL_PURPOSE"

pattern CertificateAuthorityUsageMode_SHORT_LIVED_CERTIFICATE :: CertificateAuthorityUsageMode
pattern CertificateAuthorityUsageMode_SHORT_LIVED_CERTIFICATE = CertificateAuthorityUsageMode' "SHORT_LIVED_CERTIFICATE"

{-# COMPLETE
  CertificateAuthorityUsageMode_GENERAL_PURPOSE,
  CertificateAuthorityUsageMode_SHORT_LIVED_CERTIFICATE,
  CertificateAuthorityUsageMode'
  #-}
