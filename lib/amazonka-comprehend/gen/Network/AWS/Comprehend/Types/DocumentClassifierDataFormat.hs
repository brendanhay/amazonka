-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.DocumentClassifierDataFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.DocumentClassifierDataFormat
  ( DocumentClassifierDataFormat
      ( DocumentClassifierDataFormat',
        DCDFAugmentedManifest,
        DCDFComprehendCSV
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype DocumentClassifierDataFormat = DocumentClassifierDataFormat' Lude.Text
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

pattern DCDFAugmentedManifest :: DocumentClassifierDataFormat
pattern DCDFAugmentedManifest = DocumentClassifierDataFormat' "AUGMENTED_MANIFEST"

pattern DCDFComprehendCSV :: DocumentClassifierDataFormat
pattern DCDFComprehendCSV = DocumentClassifierDataFormat' "COMPREHEND_CSV"

{-# COMPLETE
  DCDFAugmentedManifest,
  DCDFComprehendCSV,
  DocumentClassifierDataFormat'
  #-}
