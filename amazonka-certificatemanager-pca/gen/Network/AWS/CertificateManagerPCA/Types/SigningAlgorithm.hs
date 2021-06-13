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
-- Module      : Network.AWS.CertificateManagerPCA.Types.SigningAlgorithm
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManagerPCA.Types.SigningAlgorithm
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype SigningAlgorithm = SigningAlgorithm'
  { fromSigningAlgorithm ::
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
