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
-- Module      : Network.AWS.Pinpoint.Types.State
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.State
  ( State
      ( ..,
        State_ACTIVE,
        State_CANCELLED,
        State_CLOSED,
        State_COMPLETED,
        State_DRAFT
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype State = State' {fromState :: Core.Text}
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

pattern State_ACTIVE :: State
pattern State_ACTIVE = State' "ACTIVE"

pattern State_CANCELLED :: State
pattern State_CANCELLED = State' "CANCELLED"

pattern State_CLOSED :: State
pattern State_CLOSED = State' "CLOSED"

pattern State_COMPLETED :: State
pattern State_COMPLETED = State' "COMPLETED"

pattern State_DRAFT :: State
pattern State_DRAFT = State' "DRAFT"

{-# COMPLETE
  State_ACTIVE,
  State_CANCELLED,
  State_CLOSED,
  State_COMPLETED,
  State_DRAFT,
  State'
  #-}
