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
-- Module      : Network.AWS.ElasticSearch.Types.AutoTuneDesiredState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.AutoTuneDesiredState
  ( AutoTuneDesiredState
      ( ..,
        AutoTuneDesiredState_DISABLED,
        AutoTuneDesiredState_ENABLED
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Specifies the Auto-Tune desired state. Valid values are ENABLED,
-- DISABLED.
newtype AutoTuneDesiredState = AutoTuneDesiredState'
  { fromAutoTuneDesiredState ::
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

pattern AutoTuneDesiredState_DISABLED :: AutoTuneDesiredState
pattern AutoTuneDesiredState_DISABLED = AutoTuneDesiredState' "DISABLED"

pattern AutoTuneDesiredState_ENABLED :: AutoTuneDesiredState
pattern AutoTuneDesiredState_ENABLED = AutoTuneDesiredState' "ENABLED"

{-# COMPLETE
  AutoTuneDesiredState_DISABLED,
  AutoTuneDesiredState_ENABLED,
  AutoTuneDesiredState'
  #-}
