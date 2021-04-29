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

import qualified Network.AWS.Prelude as Prelude

newtype Platform = Platform'
  { fromPlatform ::
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
