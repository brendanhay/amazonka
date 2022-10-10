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
-- Module      : Amazonka.ChimeSDKMessaging.Types.AllowNotifications
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSDKMessaging.Types.AllowNotifications
  ( AllowNotifications
      ( ..,
        AllowNotifications_ALL,
        AllowNotifications_FILTERED,
        AllowNotifications_NONE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype AllowNotifications = AllowNotifications'
  { fromAllowNotifications ::
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

pattern AllowNotifications_ALL :: AllowNotifications
pattern AllowNotifications_ALL = AllowNotifications' "ALL"

pattern AllowNotifications_FILTERED :: AllowNotifications
pattern AllowNotifications_FILTERED = AllowNotifications' "FILTERED"

pattern AllowNotifications_NONE :: AllowNotifications
pattern AllowNotifications_NONE = AllowNotifications' "NONE"

{-# COMPLETE
  AllowNotifications_ALL,
  AllowNotifications_FILTERED,
  AllowNotifications_NONE,
  AllowNotifications'
  #-}
