{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.Types.SigningAlgorithm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CertificateManagerPCA.Types.SigningAlgorithm
  ( SigningAlgorithm
    ( SigningAlgorithm'
    , SigningAlgorithmSHA256WITHECDSA
    , SigningAlgorithmSHA384WITHECDSA
    , SigningAlgorithmSHA512WITHECDSA
    , SigningAlgorithmSHA256WITHRSA
    , SigningAlgorithmSHA384WITHRSA
    , SigningAlgorithmSHA512WITHRSA
    , fromSigningAlgorithm
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype SigningAlgorithm = SigningAlgorithm'{fromSigningAlgorithm
                                             :: Core.Text}
                             deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                             Core.Generic)
                             deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                               Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                               Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                               Core.FromText, Core.ToByteString, Core.ToQuery,
                                               Core.ToHeader)

pattern SigningAlgorithmSHA256WITHECDSA :: SigningAlgorithm
pattern SigningAlgorithmSHA256WITHECDSA = SigningAlgorithm' "SHA256WITHECDSA"

pattern SigningAlgorithmSHA384WITHECDSA :: SigningAlgorithm
pattern SigningAlgorithmSHA384WITHECDSA = SigningAlgorithm' "SHA384WITHECDSA"

pattern SigningAlgorithmSHA512WITHECDSA :: SigningAlgorithm
pattern SigningAlgorithmSHA512WITHECDSA = SigningAlgorithm' "SHA512WITHECDSA"

pattern SigningAlgorithmSHA256WITHRSA :: SigningAlgorithm
pattern SigningAlgorithmSHA256WITHRSA = SigningAlgorithm' "SHA256WITHRSA"

pattern SigningAlgorithmSHA384WITHRSA :: SigningAlgorithm
pattern SigningAlgorithmSHA384WITHRSA = SigningAlgorithm' "SHA384WITHRSA"

pattern SigningAlgorithmSHA512WITHRSA :: SigningAlgorithm
pattern SigningAlgorithmSHA512WITHRSA = SigningAlgorithm' "SHA512WITHRSA"

{-# COMPLETE 
  SigningAlgorithmSHA256WITHECDSA,

  SigningAlgorithmSHA384WITHECDSA,

  SigningAlgorithmSHA512WITHECDSA,

  SigningAlgorithmSHA256WITHRSA,

  SigningAlgorithmSHA384WITHRSA,

  SigningAlgorithmSHA512WITHRSA,
  SigningAlgorithm'
  #-}
