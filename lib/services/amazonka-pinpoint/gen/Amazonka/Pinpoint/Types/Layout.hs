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
-- Module      : Amazonka.Pinpoint.Types.Layout
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.Layout
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype Layout = Layout' {fromLayout :: Data.Text}
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
