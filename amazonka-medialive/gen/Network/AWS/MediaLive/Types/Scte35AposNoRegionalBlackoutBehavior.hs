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
-- Module      : Network.AWS.MediaLive.Types.Scte35AposNoRegionalBlackoutBehavior
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte35AposNoRegionalBlackoutBehavior
  ( Scte35AposNoRegionalBlackoutBehavior
      ( ..,
        Scte35AposNoRegionalBlackoutBehavior_FOLLOW,
        Scte35AposNoRegionalBlackoutBehavior_IGNORE
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Scte35 Apos No Regional Blackout Behavior
newtype Scte35AposNoRegionalBlackoutBehavior = Scte35AposNoRegionalBlackoutBehavior'
  { fromScte35AposNoRegionalBlackoutBehavior ::
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

pattern Scte35AposNoRegionalBlackoutBehavior_FOLLOW :: Scte35AposNoRegionalBlackoutBehavior
pattern Scte35AposNoRegionalBlackoutBehavior_FOLLOW = Scte35AposNoRegionalBlackoutBehavior' "FOLLOW"

pattern Scte35AposNoRegionalBlackoutBehavior_IGNORE :: Scte35AposNoRegionalBlackoutBehavior
pattern Scte35AposNoRegionalBlackoutBehavior_IGNORE = Scte35AposNoRegionalBlackoutBehavior' "IGNORE"

{-# COMPLETE
  Scte35AposNoRegionalBlackoutBehavior_FOLLOW,
  Scte35AposNoRegionalBlackoutBehavior_IGNORE,
  Scte35AposNoRegionalBlackoutBehavior'
  #-}
