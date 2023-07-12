{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SageMaker.Types.S3DataType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.S3DataType
  ( S3DataType
      ( ..,
        S3DataType_AugmentedManifestFile,
        S3DataType_ManifestFile,
        S3DataType_S3Prefix
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype S3DataType = S3DataType'
  { fromS3DataType ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
