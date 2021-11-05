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
-- Module      : Network.AWS.QuickSight.Types.ThemeType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.QuickSight.Types.ThemeType
  ( ThemeType
      ( ..,
        ThemeType_ALL,
        ThemeType_CUSTOM,
        ThemeType_QUICKSIGHT
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ThemeType = ThemeType'
  { fromThemeType ::
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

pattern ThemeType_ALL :: ThemeType
pattern ThemeType_ALL = ThemeType' "ALL"

pattern ThemeType_CUSTOM :: ThemeType
pattern ThemeType_CUSTOM = ThemeType' "CUSTOM"

pattern ThemeType_QUICKSIGHT :: ThemeType
pattern ThemeType_QUICKSIGHT = ThemeType' "QUICKSIGHT"

{-# COMPLETE
  ThemeType_ALL,
  ThemeType_CUSTOM,
  ThemeType_QUICKSIGHT,
  ThemeType'
  #-}
