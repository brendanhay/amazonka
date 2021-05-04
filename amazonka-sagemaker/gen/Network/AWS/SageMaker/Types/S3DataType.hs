{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.S3DataType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.S3DataType
  ( S3DataType
      ( ..,
        S3DataType_AugmentedManifestFile,
        S3DataType_ManifestFile,
        S3DataType_S3Prefix
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype S3DataType = S3DataType'
  { fromS3DataType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern S3DataType_AugmentedManifestFile :: S3DataType
pattern S3DataType_AugmentedManifestFile = S3DataType' "AugmentedManifestFile"

pattern S3DataType_ManifestFile :: S3DataType
pattern S3DataType_ManifestFile = S3DataType' "ManifestFile"

pattern S3DataType_S3Prefix :: S3DataType
pattern S3DataType_S3Prefix = S3DataType' "S3Prefix"

{-# COMPLETE
  S3DataType_AugmentedManifestFile,
  S3DataType_ManifestFile,
  S3DataType_S3Prefix,
  S3DataType'
  #-}
