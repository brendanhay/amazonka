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
-- Module      : Network.AWS.SageMaker.Types.AutoMLS3DataType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AutoMLS3DataType
  ( AutoMLS3DataType
      ( ..,
        AutoMLS3DataType_ManifestFile,
        AutoMLS3DataType_S3Prefix
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype AutoMLS3DataType = AutoMLS3DataType'
  { fromAutoMLS3DataType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern AutoMLS3DataType_ManifestFile :: AutoMLS3DataType
pattern AutoMLS3DataType_ManifestFile = AutoMLS3DataType' "ManifestFile"

pattern AutoMLS3DataType_S3Prefix :: AutoMLS3DataType
pattern AutoMLS3DataType_S3Prefix = AutoMLS3DataType' "S3Prefix"

{-# COMPLETE
  AutoMLS3DataType_ManifestFile,
  AutoMLS3DataType_S3Prefix,
  AutoMLS3DataType'
  #-}
