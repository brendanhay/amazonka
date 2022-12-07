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
-- Module      : Amazonka.CertificateManager.Types.KeyUsageName
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CertificateManager.Types.KeyUsageName
  ( KeyUsageName
      ( ..,
        KeyUsageName_ANY,
        KeyUsageName_CERTIFICATE_SIGNING,
        KeyUsageName_CRL_SIGNING,
        KeyUsageName_CUSTOM,
        KeyUsageName_DATA_ENCIPHERMENT,
        KeyUsageName_DECIPHER_ONLY,
        KeyUsageName_DIGITAL_SIGNATURE,
        KeyUsageName_ENCIPHER_ONLY,
        KeyUsageName_KEY_AGREEMENT,
        KeyUsageName_KEY_ENCIPHERMENT,
        KeyUsageName_NON_REPUDIATION
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype KeyUsageName = KeyUsageName'
  { fromKeyUsageName ::
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

pattern KeyUsageName_ANY :: KeyUsageName
pattern KeyUsageName_ANY = KeyUsageName' "ANY"

pattern KeyUsageName_CERTIFICATE_SIGNING :: KeyUsageName
pattern KeyUsageName_CERTIFICATE_SIGNING = KeyUsageName' "CERTIFICATE_SIGNING"

pattern KeyUsageName_CRL_SIGNING :: KeyUsageName
pattern KeyUsageName_CRL_SIGNING = KeyUsageName' "CRL_SIGNING"

pattern KeyUsageName_CUSTOM :: KeyUsageName
pattern KeyUsageName_CUSTOM = KeyUsageName' "CUSTOM"

pattern KeyUsageName_DATA_ENCIPHERMENT :: KeyUsageName
pattern KeyUsageName_DATA_ENCIPHERMENT = KeyUsageName' "DATA_ENCIPHERMENT"

pattern KeyUsageName_DECIPHER_ONLY :: KeyUsageName
pattern KeyUsageName_DECIPHER_ONLY = KeyUsageName' "DECIPHER_ONLY"

pattern KeyUsageName_DIGITAL_SIGNATURE :: KeyUsageName
pattern KeyUsageName_DIGITAL_SIGNATURE = KeyUsageName' "DIGITAL_SIGNATURE"

pattern KeyUsageName_ENCIPHER_ONLY :: KeyUsageName
pattern KeyUsageName_ENCIPHER_ONLY = KeyUsageName' "ENCIPHER_ONLY"

pattern KeyUsageName_KEY_AGREEMENT :: KeyUsageName
pattern KeyUsageName_KEY_AGREEMENT = KeyUsageName' "KEY_AGREEMENT"

pattern KeyUsageName_KEY_ENCIPHERMENT :: KeyUsageName
pattern KeyUsageName_KEY_ENCIPHERMENT = KeyUsageName' "KEY_ENCIPHERMENT"

pattern KeyUsageName_NON_REPUDIATION :: KeyUsageName
pattern KeyUsageName_NON_REPUDIATION = KeyUsageName' "NON_REPUDIATION"

{-# COMPLETE
  KeyUsageName_ANY,
  KeyUsageName_CERTIFICATE_SIGNING,
  KeyUsageName_CRL_SIGNING,
  KeyUsageName_CUSTOM,
  KeyUsageName_DATA_ENCIPHERMENT,
  KeyUsageName_DECIPHER_ONLY,
  KeyUsageName_DIGITAL_SIGNATURE,
  KeyUsageName_ENCIPHER_ONLY,
  KeyUsageName_KEY_AGREEMENT,
  KeyUsageName_KEY_ENCIPHERMENT,
  KeyUsageName_NON_REPUDIATION,
  KeyUsageName'
  #-}
