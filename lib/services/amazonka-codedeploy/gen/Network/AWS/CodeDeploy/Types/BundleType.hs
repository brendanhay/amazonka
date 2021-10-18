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
-- Module      : Network.AWS.CodeDeploy.Types.BundleType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.BundleType
  ( BundleType
      ( ..,
        BundleType_JSON,
        BundleType_Tar,
        BundleType_Tgz,
        BundleType_YAML,
        BundleType_Zip
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype BundleType = BundleType'
  { fromBundleType ::
      Core.Text
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

pattern BundleType_JSON :: BundleType
pattern BundleType_JSON = BundleType' "JSON"

pattern BundleType_Tar :: BundleType
pattern BundleType_Tar = BundleType' "tar"

pattern BundleType_Tgz :: BundleType
pattern BundleType_Tgz = BundleType' "tgz"

pattern BundleType_YAML :: BundleType
pattern BundleType_YAML = BundleType' "YAML"

pattern BundleType_Zip :: BundleType
pattern BundleType_Zip = BundleType' "zip"

{-# COMPLETE
  BundleType_JSON,
  BundleType_Tar,
  BundleType_Tgz,
  BundleType_YAML,
  BundleType_Zip,
  BundleType'
  #-}
