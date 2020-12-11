-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.HlsKeyProviderType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.HlsKeyProviderType
  ( HlsKeyProviderType
      ( HlsKeyProviderType',
        HKPTSpeke,
        HKPTStaticKey
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Specify whether your DRM encryption key is static or from a key provider that follows the SPEKE standard. For more information about SPEKE, see https://docs.aws.amazon.com/speke/latest/documentation/what-is-speke.html.
newtype HlsKeyProviderType = HlsKeyProviderType' Lude.Text
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

pattern HKPTSpeke :: HlsKeyProviderType
pattern HKPTSpeke = HlsKeyProviderType' "SPEKE"

pattern HKPTStaticKey :: HlsKeyProviderType
pattern HKPTStaticKey = HlsKeyProviderType' "STATIC_KEY"

{-# COMPLETE
  HKPTSpeke,
  HKPTStaticKey,
  HlsKeyProviderType'
  #-}
