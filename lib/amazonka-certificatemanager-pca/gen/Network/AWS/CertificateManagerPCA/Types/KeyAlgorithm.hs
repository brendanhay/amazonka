{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.Types.KeyAlgorithm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CertificateManagerPCA.Types.KeyAlgorithm
  ( KeyAlgorithm
    ( KeyAlgorithm'
    , KeyAlgorithmRsa2048
    , KeyAlgorithmRsa4096
    , KeyAlgorithmEcPRIME256V1
    , KeyAlgorithmEcSECP384R1
    , fromKeyAlgorithm
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype KeyAlgorithm = KeyAlgorithm'{fromKeyAlgorithm :: Core.Text}
                         deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                         Core.Generic)
                         deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                           Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                           Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                           Core.FromText, Core.ToByteString, Core.ToQuery,
                                           Core.ToHeader)

pattern KeyAlgorithmRsa2048 :: KeyAlgorithm
pattern KeyAlgorithmRsa2048 = KeyAlgorithm' "RSA_2048"

pattern KeyAlgorithmRsa4096 :: KeyAlgorithm
pattern KeyAlgorithmRsa4096 = KeyAlgorithm' "RSA_4096"

pattern KeyAlgorithmEcPRIME256V1 :: KeyAlgorithm
pattern KeyAlgorithmEcPRIME256V1 = KeyAlgorithm' "EC_prime256v1"

pattern KeyAlgorithmEcSECP384R1 :: KeyAlgorithm
pattern KeyAlgorithmEcSECP384R1 = KeyAlgorithm' "EC_secp384r1"

{-# COMPLETE 
  KeyAlgorithmRsa2048,

  KeyAlgorithmRsa4096,

  KeyAlgorithmEcPRIME256V1,

  KeyAlgorithmEcSECP384R1,
  KeyAlgorithm'
  #-}
