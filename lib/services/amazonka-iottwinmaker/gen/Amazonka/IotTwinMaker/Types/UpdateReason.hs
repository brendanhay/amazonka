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
-- Module      : Amazonka.IotTwinMaker.Types.UpdateReason
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Types.UpdateReason
  ( UpdateReason
      ( ..,
        UpdateReason_DEFAULT,
        UpdateReason_ENTITY_COUNT_UPDATE,
        UpdateReason_OVERWRITTEN,
        UpdateReason_PRICING_MODE_UPDATE,
        UpdateReason_PRICING_TIER_UPDATE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype UpdateReason = UpdateReason'
  { fromUpdateReason ::
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

pattern UpdateReason_DEFAULT :: UpdateReason
pattern UpdateReason_DEFAULT = UpdateReason' "DEFAULT"

pattern UpdateReason_ENTITY_COUNT_UPDATE :: UpdateReason
pattern UpdateReason_ENTITY_COUNT_UPDATE = UpdateReason' "ENTITY_COUNT_UPDATE"

pattern UpdateReason_OVERWRITTEN :: UpdateReason
pattern UpdateReason_OVERWRITTEN = UpdateReason' "OVERWRITTEN"

pattern UpdateReason_PRICING_MODE_UPDATE :: UpdateReason
pattern UpdateReason_PRICING_MODE_UPDATE = UpdateReason' "PRICING_MODE_UPDATE"

pattern UpdateReason_PRICING_TIER_UPDATE :: UpdateReason
pattern UpdateReason_PRICING_TIER_UPDATE = UpdateReason' "PRICING_TIER_UPDATE"

{-# COMPLETE
  UpdateReason_DEFAULT,
  UpdateReason_ENTITY_COUNT_UPDATE,
  UpdateReason_OVERWRITTEN,
  UpdateReason_PRICING_MODE_UPDATE,
  UpdateReason_PRICING_TIER_UPDATE,
  UpdateReason'
  #-}
