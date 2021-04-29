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

import qualified Network.AWS.Prelude as Prelude

newtype AutoMLS3DataType = AutoMLS3DataType'
  { fromAutoMLS3DataType ::
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

pattern AutoMLS3DataType_ManifestFile :: AutoMLS3DataType
pattern AutoMLS3DataType_ManifestFile = AutoMLS3DataType' "ManifestFile"

pattern AutoMLS3DataType_S3Prefix :: AutoMLS3DataType
pattern AutoMLS3DataType_S3Prefix = AutoMLS3DataType' "S3Prefix"

{-# COMPLETE
  AutoMLS3DataType_ManifestFile,
  AutoMLS3DataType_S3Prefix,
  AutoMLS3DataType'
  #-}
