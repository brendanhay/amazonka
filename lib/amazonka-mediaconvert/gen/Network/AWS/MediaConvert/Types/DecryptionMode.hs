{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DecryptionMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DecryptionMode
  ( DecryptionMode
      ( DecryptionMode',
        DecryptionModeAesCtr,
        DecryptionModeAesCbc,
        DecryptionModeAesGcm,
        fromDecryptionMode
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Specify the encryption mode that you used to encrypt your input files.
newtype DecryptionMode = DecryptionMode'
  { fromDecryptionMode ::
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

pattern DecryptionModeAesCtr :: DecryptionMode
pattern DecryptionModeAesCtr = DecryptionMode' "AES_CTR"

pattern DecryptionModeAesCbc :: DecryptionMode
pattern DecryptionModeAesCbc = DecryptionMode' "AES_CBC"

pattern DecryptionModeAesGcm :: DecryptionMode
pattern DecryptionModeAesGcm = DecryptionMode' "AES_GCM"

{-# COMPLETE
  DecryptionModeAesCtr,
  DecryptionModeAesCbc,
  DecryptionModeAesGcm,
  DecryptionMode'
  #-}
