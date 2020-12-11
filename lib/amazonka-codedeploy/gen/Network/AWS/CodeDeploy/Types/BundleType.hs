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
        JSON,
        TAR,
        TGZ,
        Yaml,
        Zip
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype BundleType = BundleType' Lude.Text
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

pattern JSON :: BundleType
pattern JSON = BundleType' "JSON"

pattern TAR :: BundleType
pattern TAR = BundleType' "tar"

pattern TGZ :: BundleType
pattern TGZ = BundleType' "tgz"

pattern Yaml :: BundleType
pattern Yaml = BundleType' "YAML"

pattern Zip :: BundleType
pattern Zip = BundleType' "zip"

{-# COMPLETE
  JSON,
  TAR,
  TGZ,
  Yaml,
  Zip,
  BundleType'
  #-}
