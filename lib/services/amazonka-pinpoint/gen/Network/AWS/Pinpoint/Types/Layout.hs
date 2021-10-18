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
-- Module      : Network.AWS.Pinpoint.Types.Layout
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.Layout
  ( Layout
      ( ..,
        Layout_BOTTOM_BANNER,
        Layout_CAROUSEL,
        Layout_MIDDLE_BANNER,
        Layout_MOBILE_FEED,
        Layout_OVERLAYS,
        Layout_TOP_BANNER
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype Layout = Layout' {fromLayout :: Core.Text}
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

pattern Layout_BOTTOM_BANNER :: Layout
pattern Layout_BOTTOM_BANNER = Layout' "BOTTOM_BANNER"

pattern Layout_CAROUSEL :: Layout
pattern Layout_CAROUSEL = Layout' "CAROUSEL"

pattern Layout_MIDDLE_BANNER :: Layout
pattern Layout_MIDDLE_BANNER = Layout' "MIDDLE_BANNER"

pattern Layout_MOBILE_FEED :: Layout
pattern Layout_MOBILE_FEED = Layout' "MOBILE_FEED"

pattern Layout_OVERLAYS :: Layout
pattern Layout_OVERLAYS = Layout' "OVERLAYS"

pattern Layout_TOP_BANNER :: Layout
pattern Layout_TOP_BANNER = Layout' "TOP_BANNER"

{-# COMPLETE
  Layout_BOTTOM_BANNER,
  Layout_CAROUSEL,
  Layout_MIDDLE_BANNER,
  Layout_MOBILE_FEED,
  Layout_OVERLAYS,
  Layout_TOP_BANNER,
  Layout'
  #-}
