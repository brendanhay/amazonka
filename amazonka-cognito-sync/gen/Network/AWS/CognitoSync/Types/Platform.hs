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
-- Module      : Network.AWS.CognitoSync.Types.Platform
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoSync.Types.Platform
  ( Platform
      ( ..,
        Platform_ADM,
        Platform_APNS,
        Platform_APNS_SANDBOX,
        Platform_GCM
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype Platform = Platform'
  { fromPlatform ::
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

pattern Platform_ADM :: Platform
pattern Platform_ADM = Platform' "ADM"

pattern Platform_APNS :: Platform
pattern Platform_APNS = Platform' "APNS"

pattern Platform_APNS_SANDBOX :: Platform
pattern Platform_APNS_SANDBOX = Platform' "APNS_SANDBOX"

pattern Platform_GCM :: Platform
pattern Platform_GCM = Platform' "GCM"

{-# COMPLETE
  Platform_ADM,
  Platform_APNS,
  Platform_APNS_SANDBOX,
  Platform_GCM,
  Platform'
  #-}
