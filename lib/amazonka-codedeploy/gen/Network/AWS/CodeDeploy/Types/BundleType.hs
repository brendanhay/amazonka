{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.BundleType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.BundleType
  ( BundleType
      ( BundleType',
        BundleTypeTar,
        BundleTypeTgz,
        BundleTypeZip,
        BundleTypeYaml,
        BundleTypeJson,
        fromBundleType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype BundleType = BundleType' {fromBundleType :: Core.Text}
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern BundleTypeTar :: BundleType
pattern BundleTypeTar = BundleType' "tar"

pattern BundleTypeTgz :: BundleType
pattern BundleTypeTgz = BundleType' "tgz"

pattern BundleTypeZip :: BundleType
pattern BundleTypeZip = BundleType' "zip"

pattern BundleTypeYaml :: BundleType
pattern BundleTypeYaml = BundleType' "YAML"

pattern BundleTypeJson :: BundleType
pattern BundleTypeJson = BundleType' "JSON"

{-# COMPLETE
  BundleTypeTar,
  BundleTypeTgz,
  BundleTypeZip,
  BundleTypeYaml,
  BundleTypeJson,
  BundleType'
  #-}
