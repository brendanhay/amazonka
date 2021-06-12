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
-- Module      : Network.AWS.MediaLive.Types.AacProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AacProfile
  ( AacProfile
      ( ..,
        AacProfile_HEV1,
        AacProfile_HEV2,
        AacProfile_LC
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Aac Profile
newtype AacProfile = AacProfile'
  { fromAacProfile ::
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

pattern AacProfile_HEV1 :: AacProfile
pattern AacProfile_HEV1 = AacProfile' "HEV1"

pattern AacProfile_HEV2 :: AacProfile
pattern AacProfile_HEV2 = AacProfile' "HEV2"

pattern AacProfile_LC :: AacProfile
pattern AacProfile_LC = AacProfile' "LC"

{-# COMPLETE
  AacProfile_HEV1,
  AacProfile_HEV2,
  AacProfile_LC,
  AacProfile'
  #-}
