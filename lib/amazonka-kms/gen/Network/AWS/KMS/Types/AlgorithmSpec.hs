{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.AlgorithmSpec
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.AlgorithmSpec
  ( AlgorithmSpec
      ( AlgorithmSpec',
        AlgorithmSpecRsaesPKCS1V15,
        AlgorithmSpecRsaesOaepSha1,
        AlgorithmSpecRsaesOaepSha256,
        fromAlgorithmSpec
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype AlgorithmSpec = AlgorithmSpec'
  { fromAlgorithmSpec ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern AlgorithmSpecRsaesPKCS1V15 :: AlgorithmSpec
pattern AlgorithmSpecRsaesPKCS1V15 = AlgorithmSpec' "RSAES_PKCS1_V1_5"

pattern AlgorithmSpecRsaesOaepSha1 :: AlgorithmSpec
pattern AlgorithmSpecRsaesOaepSha1 = AlgorithmSpec' "RSAES_OAEP_SHA_1"

pattern AlgorithmSpecRsaesOaepSha256 :: AlgorithmSpec
pattern AlgorithmSpecRsaesOaepSha256 = AlgorithmSpec' "RSAES_OAEP_SHA_256"

{-# COMPLETE
  AlgorithmSpecRsaesPKCS1V15,
  AlgorithmSpecRsaesOaepSha1,
  AlgorithmSpecRsaesOaepSha256,
  AlgorithmSpec'
  #-}
