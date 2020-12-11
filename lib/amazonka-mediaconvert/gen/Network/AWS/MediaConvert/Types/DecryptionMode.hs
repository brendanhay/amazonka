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
        DMAESCbc,
        DMAESCtr,
        DMAESGCM
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Specify the encryption mode that you used to encrypt your input files.
newtype DecryptionMode = DecryptionMode' Lude.Text
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

pattern DMAESCbc :: DecryptionMode
pattern DMAESCbc = DecryptionMode' "AES_CBC"

pattern DMAESCtr :: DecryptionMode
pattern DMAESCtr = DecryptionMode' "AES_CTR"

pattern DMAESGCM :: DecryptionMode
pattern DMAESGCM = DecryptionMode' "AES_GCM"

{-# COMPLETE
  DMAESCbc,
  DMAESCtr,
  DMAESGCM,
  DecryptionMode'
  #-}
