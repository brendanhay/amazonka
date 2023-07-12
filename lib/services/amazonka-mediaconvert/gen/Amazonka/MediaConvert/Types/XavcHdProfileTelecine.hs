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
-- Module      : Amazonka.MediaConvert.Types.XavcHdProfileTelecine
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.XavcHdProfileTelecine
  ( XavcHdProfileTelecine
      ( ..,
        XavcHdProfileTelecine_HARD,
        XavcHdProfileTelecine_NONE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Ignore this setting unless you set Frame rate (framerateNumerator
-- divided by framerateDenominator) to 29.970. If your input framerate is
-- 23.976, choose Hard (HARD). Otherwise, keep the default value None
-- (NONE). For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/working-with-telecine-and-inverse-telecine.html.
newtype XavcHdProfileTelecine = XavcHdProfileTelecine'
  { fromXavcHdProfileTelecine ::
      Data.Text
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

pattern XavcHdProfileTelecine_HARD :: XavcHdProfileTelecine
pattern XavcHdProfileTelecine_HARD = XavcHdProfileTelecine' "HARD"

pattern XavcHdProfileTelecine_NONE :: XavcHdProfileTelecine
pattern XavcHdProfileTelecine_NONE = XavcHdProfileTelecine' "NONE"

{-# COMPLETE
  XavcHdProfileTelecine_HARD,
  XavcHdProfileTelecine_NONE,
  XavcHdProfileTelecine'
  #-}
