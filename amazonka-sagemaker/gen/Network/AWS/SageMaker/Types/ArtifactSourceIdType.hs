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
-- Module      : Network.AWS.SageMaker.Types.ArtifactSourceIdType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ArtifactSourceIdType
  ( ArtifactSourceIdType
      ( ..,
        ArtifactSourceIdType_Custom,
        ArtifactSourceIdType_MD5Hash,
        ArtifactSourceIdType_S3ETag,
        ArtifactSourceIdType_S3Version
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ArtifactSourceIdType = ArtifactSourceIdType'
  { fromArtifactSourceIdType ::
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

pattern ArtifactSourceIdType_Custom :: ArtifactSourceIdType
pattern ArtifactSourceIdType_Custom = ArtifactSourceIdType' "Custom"

pattern ArtifactSourceIdType_MD5Hash :: ArtifactSourceIdType
pattern ArtifactSourceIdType_MD5Hash = ArtifactSourceIdType' "MD5Hash"

pattern ArtifactSourceIdType_S3ETag :: ArtifactSourceIdType
pattern ArtifactSourceIdType_S3ETag = ArtifactSourceIdType' "S3ETag"

pattern ArtifactSourceIdType_S3Version :: ArtifactSourceIdType
pattern ArtifactSourceIdType_S3Version = ArtifactSourceIdType' "S3Version"

{-# COMPLETE
  ArtifactSourceIdType_Custom,
  ArtifactSourceIdType_MD5Hash,
  ArtifactSourceIdType_S3ETag,
  ArtifactSourceIdType_S3Version,
  ArtifactSourceIdType'
  #-}
