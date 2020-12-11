-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.AppAttributesKeys
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.AppAttributesKeys
  ( AppAttributesKeys
      ( AppAttributesKeys',
        AWSFlowRubySettings,
        AutoBundleOnDeploy,
        DocumentRoot,
        RailsEnv
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype AppAttributesKeys = AppAttributesKeys' Lude.Text
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

pattern AWSFlowRubySettings :: AppAttributesKeys
pattern AWSFlowRubySettings = AppAttributesKeys' "AwsFlowRubySettings"

pattern AutoBundleOnDeploy :: AppAttributesKeys
pattern AutoBundleOnDeploy = AppAttributesKeys' "AutoBundleOnDeploy"

pattern DocumentRoot :: AppAttributesKeys
pattern DocumentRoot = AppAttributesKeys' "DocumentRoot"

pattern RailsEnv :: AppAttributesKeys
pattern RailsEnv = AppAttributesKeys' "RailsEnv"

{-# COMPLETE
  AWSFlowRubySettings,
  AutoBundleOnDeploy,
  DocumentRoot,
  RailsEnv,
  AppAttributesKeys'
  #-}
