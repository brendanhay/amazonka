{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.Types.SigningAlgorithm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManagerPCA.Types.SigningAlgorithm
  ( SigningAlgorithm
      ( SigningAlgorithm',
        SHA256WITHECDSA,
        SHA384WITHECDSA,
        SHA512WITHECDSA,
        SHA256WITHRSA,
        SHA384WITHRSA,
        SHA512WITHRSA
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype SigningAlgorithm = SigningAlgorithm' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern SHA256WITHECDSA :: SigningAlgorithm
pattern SHA256WITHECDSA = SigningAlgorithm' "SHA256WITHECDSA"

pattern SHA384WITHECDSA :: SigningAlgorithm
pattern SHA384WITHECDSA = SigningAlgorithm' "SHA384WITHECDSA"

pattern SHA512WITHECDSA :: SigningAlgorithm
pattern SHA512WITHECDSA = SigningAlgorithm' "SHA512WITHECDSA"

pattern SHA256WITHRSA :: SigningAlgorithm
pattern SHA256WITHRSA = SigningAlgorithm' "SHA256WITHRSA"

pattern SHA384WITHRSA :: SigningAlgorithm
pattern SHA384WITHRSA = SigningAlgorithm' "SHA384WITHRSA"

pattern SHA512WITHRSA :: SigningAlgorithm
pattern SHA512WITHRSA = SigningAlgorithm' "SHA512WITHRSA"

{-# COMPLETE
  SHA256WITHECDSA,
  SHA384WITHECDSA,
  SHA512WITHECDSA,
  SHA256WITHRSA,
  SHA384WITHRSA,
  SHA512WITHRSA,
  SigningAlgorithm'
  #-}
