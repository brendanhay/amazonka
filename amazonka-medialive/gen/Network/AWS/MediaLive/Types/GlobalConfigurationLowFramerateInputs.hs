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
-- Module      : Network.AWS.MediaLive.Types.GlobalConfigurationLowFramerateInputs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.GlobalConfigurationLowFramerateInputs
  ( GlobalConfigurationLowFramerateInputs
      ( ..,
        GlobalConfigurationLowFramerateInputs_DISABLED,
        GlobalConfigurationLowFramerateInputs_ENABLED
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Global Configuration Low Framerate Inputs
newtype GlobalConfigurationLowFramerateInputs = GlobalConfigurationLowFramerateInputs'
  { fromGlobalConfigurationLowFramerateInputs ::
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

pattern GlobalConfigurationLowFramerateInputs_DISABLED :: GlobalConfigurationLowFramerateInputs
pattern GlobalConfigurationLowFramerateInputs_DISABLED = GlobalConfigurationLowFramerateInputs' "DISABLED"

pattern GlobalConfigurationLowFramerateInputs_ENABLED :: GlobalConfigurationLowFramerateInputs
pattern GlobalConfigurationLowFramerateInputs_ENABLED = GlobalConfigurationLowFramerateInputs' "ENABLED"

{-# COMPLETE
  GlobalConfigurationLowFramerateInputs_DISABLED,
  GlobalConfigurationLowFramerateInputs_ENABLED,
  GlobalConfigurationLowFramerateInputs'
  #-}
