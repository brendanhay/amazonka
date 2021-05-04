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
-- Module      : Network.AWS.OpsWorks.Types.AppAttributesKeys
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.AppAttributesKeys
  ( AppAttributesKeys
      ( ..,
        AppAttributesKeys_AutoBundleOnDeploy,
        AppAttributesKeys_AwsFlowRubySettings,
        AppAttributesKeys_DocumentRoot,
        AppAttributesKeys_RailsEnv
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype AppAttributesKeys = AppAttributesKeys'
  { fromAppAttributesKeys ::
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

pattern AppAttributesKeys_AutoBundleOnDeploy :: AppAttributesKeys
pattern AppAttributesKeys_AutoBundleOnDeploy = AppAttributesKeys' "AutoBundleOnDeploy"

pattern AppAttributesKeys_AwsFlowRubySettings :: AppAttributesKeys
pattern AppAttributesKeys_AwsFlowRubySettings = AppAttributesKeys' "AwsFlowRubySettings"

pattern AppAttributesKeys_DocumentRoot :: AppAttributesKeys
pattern AppAttributesKeys_DocumentRoot = AppAttributesKeys' "DocumentRoot"

pattern AppAttributesKeys_RailsEnv :: AppAttributesKeys
pattern AppAttributesKeys_RailsEnv = AppAttributesKeys' "RailsEnv"

{-# COMPLETE
  AppAttributesKeys_AutoBundleOnDeploy,
  AppAttributesKeys_AwsFlowRubySettings,
  AppAttributesKeys_DocumentRoot,
  AppAttributesKeys_RailsEnv,
  AppAttributesKeys'
  #-}
