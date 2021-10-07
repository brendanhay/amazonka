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
-- Module      : Network.AWS.MediaConvert.Types.XavcHdProfileTelecine
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.XavcHdProfileTelecine
  ( XavcHdProfileTelecine
      ( ..,
        XavcHdProfileTelecine_HARD,
        XavcHdProfileTelecine_NONE
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Ignore this setting unless you set Frame rate (framerateNumerator
-- divided by framerateDenominator) to 29.970. If your input framerate is
-- 23.976, choose Hard (HARD). Otherwise, keep the default value None
-- (NONE). For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/working-with-telecine-and-inverse-telecine.html.
newtype XavcHdProfileTelecine = XavcHdProfileTelecine'
  { fromXavcHdProfileTelecine ::
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

pattern XavcHdProfileTelecine_HARD :: XavcHdProfileTelecine
pattern XavcHdProfileTelecine_HARD = XavcHdProfileTelecine' "HARD"

pattern XavcHdProfileTelecine_NONE :: XavcHdProfileTelecine
pattern XavcHdProfileTelecine_NONE = XavcHdProfileTelecine' "NONE"

{-# COMPLETE
  XavcHdProfileTelecine_HARD,
  XavcHdProfileTelecine_NONE,
  XavcHdProfileTelecine'
  #-}
