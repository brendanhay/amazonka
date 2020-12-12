{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.Types.KeyAlgorithm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManagerPCA.Types.KeyAlgorithm
  ( KeyAlgorithm
      ( KeyAlgorithm',
        EcPRIME256V1,
        EcSECP384R1,
        Rsa2048,
        Rsa4096
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype KeyAlgorithm = KeyAlgorithm' Lude.Text
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

pattern EcPRIME256V1 :: KeyAlgorithm
pattern EcPRIME256V1 = KeyAlgorithm' "EC_prime256v1"

pattern EcSECP384R1 :: KeyAlgorithm
pattern EcSECP384R1 = KeyAlgorithm' "EC_secp384r1"

pattern Rsa2048 :: KeyAlgorithm
pattern Rsa2048 = KeyAlgorithm' "RSA_2048"

pattern Rsa4096 :: KeyAlgorithm
pattern Rsa4096 = KeyAlgorithm' "RSA_4096"

{-# COMPLETE
  EcPRIME256V1,
  EcSECP384R1,
  Rsa2048,
  Rsa4096,
  KeyAlgorithm'
  #-}
