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
-- Module      : Amazonka.CertificateManagerPCA.Types.SigningAlgorithm
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CertificateManagerPCA.Types.SigningAlgorithm
  ( SigningAlgorithm
      ( ..,
        SigningAlgorithm_SHA256WITHECDSA,
        SigningAlgorithm_SHA256WITHRSA,
        SigningAlgorithm_SHA384WITHECDSA,
        SigningAlgorithm_SHA384WITHRSA,
        SigningAlgorithm_SHA512WITHECDSA,
        SigningAlgorithm_SHA512WITHRSA
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SigningAlgorithm = SigningAlgorithm'
  { fromSigningAlgorithm ::
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

pattern SigningAlgorithm_SHA256WITHECDSA :: SigningAlgorithm
pattern SigningAlgorithm_SHA256WITHECDSA = SigningAlgorithm' "SHA256WITHECDSA"

pattern SigningAlgorithm_SHA256WITHRSA :: SigningAlgorithm
pattern SigningAlgorithm_SHA256WITHRSA = SigningAlgorithm' "SHA256WITHRSA"

pattern SigningAlgorithm_SHA384WITHECDSA :: SigningAlgorithm
pattern SigningAlgorithm_SHA384WITHECDSA = SigningAlgorithm' "SHA384WITHECDSA"

pattern SigningAlgorithm_SHA384WITHRSA :: SigningAlgorithm
pattern SigningAlgorithm_SHA384WITHRSA = SigningAlgorithm' "SHA384WITHRSA"

pattern SigningAlgorithm_SHA512WITHECDSA :: SigningAlgorithm
pattern SigningAlgorithm_SHA512WITHECDSA = SigningAlgorithm' "SHA512WITHECDSA"

pattern SigningAlgorithm_SHA512WITHRSA :: SigningAlgorithm
pattern SigningAlgorithm_SHA512WITHRSA = SigningAlgorithm' "SHA512WITHRSA"

{-# COMPLETE
  SigningAlgorithm_SHA256WITHECDSA,
  SigningAlgorithm_SHA256WITHRSA,
  SigningAlgorithm_SHA384WITHECDSA,
  SigningAlgorithm_SHA384WITHRSA,
  SigningAlgorithm_SHA512WITHECDSA,
  SigningAlgorithm_SHA512WITHRSA,
  SigningAlgorithm'
  #-}
