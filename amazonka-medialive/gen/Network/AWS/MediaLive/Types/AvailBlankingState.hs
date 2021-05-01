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
-- Module      : Network.AWS.MediaLive.Types.AvailBlankingState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AvailBlankingState
  ( AvailBlankingState
      ( ..,
        AvailBlankingState_DISABLED,
        AvailBlankingState_ENABLED
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Avail Blanking State
newtype AvailBlankingState = AvailBlankingState'
  { fromAvailBlankingState ::
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

pattern AvailBlankingState_DISABLED :: AvailBlankingState
pattern AvailBlankingState_DISABLED = AvailBlankingState' "DISABLED"

pattern AvailBlankingState_ENABLED :: AvailBlankingState
pattern AvailBlankingState_ENABLED = AvailBlankingState' "ENABLED"

{-# COMPLETE
  AvailBlankingState_DISABLED,
  AvailBlankingState_ENABLED,
  AvailBlankingState'
  #-}
