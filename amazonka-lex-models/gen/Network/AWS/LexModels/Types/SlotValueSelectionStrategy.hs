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
-- Module      : Network.AWS.LexModels.Types.SlotValueSelectionStrategy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.SlotValueSelectionStrategy
  ( SlotValueSelectionStrategy
      ( ..,
        SlotValueSelectionStrategy_ORIGINAL_VALUE,
        SlotValueSelectionStrategy_TOP_RESOLUTION
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype SlotValueSelectionStrategy = SlotValueSelectionStrategy'
  { fromSlotValueSelectionStrategy ::
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

pattern SlotValueSelectionStrategy_ORIGINAL_VALUE :: SlotValueSelectionStrategy
pattern SlotValueSelectionStrategy_ORIGINAL_VALUE = SlotValueSelectionStrategy' "ORIGINAL_VALUE"

pattern SlotValueSelectionStrategy_TOP_RESOLUTION :: SlotValueSelectionStrategy
pattern SlotValueSelectionStrategy_TOP_RESOLUTION = SlotValueSelectionStrategy' "TOP_RESOLUTION"

{-# COMPLETE
  SlotValueSelectionStrategy_ORIGINAL_VALUE,
  SlotValueSelectionStrategy_TOP_RESOLUTION,
  SlotValueSelectionStrategy'
  #-}
