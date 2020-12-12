{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.S3DataType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.S3DataType
  ( S3DataType
      ( S3DataType',
        AugmentedManifestFile,
        ManifestFile,
        S3Prefix
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype S3DataType = S3DataType' Lude.Text
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

pattern AugmentedManifestFile :: S3DataType
pattern AugmentedManifestFile = S3DataType' "AugmentedManifestFile"

pattern ManifestFile :: S3DataType
pattern ManifestFile = S3DataType' "ManifestFile"

pattern S3Prefix :: S3DataType
pattern S3Prefix = S3DataType' "S3Prefix"

{-# COMPLETE
  AugmentedManifestFile,
  ManifestFile,
  S3Prefix,
  S3DataType'
  #-}
