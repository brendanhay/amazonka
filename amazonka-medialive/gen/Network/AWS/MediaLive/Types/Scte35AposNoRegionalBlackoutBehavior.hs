{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

-- | Scte35 Apos No Regional Blackout Behavior
newtype Scte35AposNoRegionalBlackoutBehavior = Scte35AposNoRegionalBlackoutBehavior'
  { fromScte35AposNoRegionalBlackoutBehavior ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
