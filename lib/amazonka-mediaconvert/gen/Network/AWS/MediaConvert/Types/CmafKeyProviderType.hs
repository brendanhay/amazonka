-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CmafKeyProviderType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CmafKeyProviderType
  ( CmafKeyProviderType
      ( CmafKeyProviderType',
        Speke,
        StaticKey
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Specify whether your DRM encryption key is static or from a key provider that follows the SPEKE standard. For more information about SPEKE, see https://docs.aws.amazon.com/speke/latest/documentation/what-is-speke.html.
newtype CmafKeyProviderType = CmafKeyProviderType' Lude.Text
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

pattern Speke :: CmafKeyProviderType
pattern Speke = CmafKeyProviderType' "SPEKE"

pattern StaticKey :: CmafKeyProviderType
pattern StaticKey = CmafKeyProviderType' "STATIC_KEY"

{-# COMPLETE
  Speke,
  StaticKey,
  CmafKeyProviderType'
  #-}
