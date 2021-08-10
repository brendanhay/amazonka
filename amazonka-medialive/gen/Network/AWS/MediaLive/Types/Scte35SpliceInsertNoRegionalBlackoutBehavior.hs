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
-- Module      : Network.AWS.MediaLive.Types.Scte35SpliceInsertNoRegionalBlackoutBehavior
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte35SpliceInsertNoRegionalBlackoutBehavior
  ( Scte35SpliceInsertNoRegionalBlackoutBehavior
      ( ..,
        Scte35SpliceInsertNoRegionalBlackoutBehavior_FOLLOW,
        Scte35SpliceInsertNoRegionalBlackoutBehavior_IGNORE
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Scte35 Splice Insert No Regional Blackout Behavior
newtype Scte35SpliceInsertNoRegionalBlackoutBehavior = Scte35SpliceInsertNoRegionalBlackoutBehavior'
  { fromScte35SpliceInsertNoRegionalBlackoutBehavior ::
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

pattern Scte35SpliceInsertNoRegionalBlackoutBehavior_FOLLOW :: Scte35SpliceInsertNoRegionalBlackoutBehavior
pattern Scte35SpliceInsertNoRegionalBlackoutBehavior_FOLLOW = Scte35SpliceInsertNoRegionalBlackoutBehavior' "FOLLOW"

pattern Scte35SpliceInsertNoRegionalBlackoutBehavior_IGNORE :: Scte35SpliceInsertNoRegionalBlackoutBehavior
pattern Scte35SpliceInsertNoRegionalBlackoutBehavior_IGNORE = Scte35SpliceInsertNoRegionalBlackoutBehavior' "IGNORE"

{-# COMPLETE
  Scte35SpliceInsertNoRegionalBlackoutBehavior_FOLLOW,
  Scte35SpliceInsertNoRegionalBlackoutBehavior_IGNORE,
  Scte35SpliceInsertNoRegionalBlackoutBehavior'
  #-}
