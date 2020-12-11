-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLS3DataType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AutoMLS3DataType
  ( AutoMLS3DataType
      ( AutoMLS3DataType',
        AMLSDTManifestFile,
        AMLSDTS3Prefix
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype AutoMLS3DataType = AutoMLS3DataType' Lude.Text
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

pattern AMLSDTManifestFile :: AutoMLS3DataType
pattern AMLSDTManifestFile = AutoMLS3DataType' "ManifestFile"

pattern AMLSDTS3Prefix :: AutoMLS3DataType
pattern AMLSDTS3Prefix = AutoMLS3DataType' "S3Prefix"

{-# COMPLETE
  AMLSDTManifestFile,
  AMLSDTS3Prefix,
  AutoMLS3DataType'
  #-}
