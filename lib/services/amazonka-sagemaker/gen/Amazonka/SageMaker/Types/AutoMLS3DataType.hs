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
-- Module      : Amazonka.SageMaker.Types.AutoMLS3DataType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.AutoMLS3DataType
  ( AutoMLS3DataType
      ( ..,
        AutoMLS3DataType_AugmentedManifestFile,
        AutoMLS3DataType_ManifestFile,
        AutoMLS3DataType_S3Prefix
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AutoMLS3DataType = AutoMLS3DataType'
  { fromAutoMLS3DataType ::
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

pattern AutoMLS3DataType_AugmentedManifestFile :: AutoMLS3DataType
pattern AutoMLS3DataType_AugmentedManifestFile = AutoMLS3DataType' "AugmentedManifestFile"

pattern AutoMLS3DataType_ManifestFile :: AutoMLS3DataType
pattern AutoMLS3DataType_ManifestFile = AutoMLS3DataType' "ManifestFile"

pattern AutoMLS3DataType_S3Prefix :: AutoMLS3DataType
pattern AutoMLS3DataType_S3Prefix = AutoMLS3DataType' "S3Prefix"

{-# COMPLETE
  AutoMLS3DataType_AugmentedManifestFile,
  AutoMLS3DataType_ManifestFile,
  AutoMLS3DataType_S3Prefix,
  AutoMLS3DataType'
  #-}
