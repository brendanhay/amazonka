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
-- Module      : Network.AWS.Pinpoint.Types.Duration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.Duration
  ( Duration
      ( ..,
        Duration_DAY_14,
        Duration_DAY_30,
        Duration_DAY_7,
        Duration_HR_24
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype Duration = Duration'
  { fromDuration ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern Duration_DAY_14 :: Duration
pattern Duration_DAY_14 = Duration' "DAY_14"

pattern Duration_DAY_30 :: Duration
pattern Duration_DAY_30 = Duration' "DAY_30"

pattern Duration_DAY_7 :: Duration
pattern Duration_DAY_7 = Duration' "DAY_7"

pattern Duration_HR_24 :: Duration
pattern Duration_HR_24 = Duration' "HR_24"

{-# COMPLETE
  Duration_DAY_14,
  Duration_DAY_30,
  Duration_DAY_7,
  Duration_HR_24,
  Duration'
  #-}
