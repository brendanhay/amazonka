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
-- Module      : Network.AWS.MediaConvert.Types.DolbyVisionProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DolbyVisionProfile
  ( DolbyVisionProfile
      ( ..,
        DolbyVisionProfile_PROFILE_5
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | In the current MediaConvert implementation, the Dolby Vision profile is
-- always 5 (PROFILE_5). Therefore, all of your inputs must contain Dolby
-- Vision frame interleaved data.
newtype DolbyVisionProfile = DolbyVisionProfile'
  { fromDolbyVisionProfile ::
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

pattern DolbyVisionProfile_PROFILE_5 :: DolbyVisionProfile
pattern DolbyVisionProfile_PROFILE_5 = DolbyVisionProfile' "PROFILE_5"

{-# COMPLETE
  DolbyVisionProfile_PROFILE_5,
  DolbyVisionProfile'
  #-}
