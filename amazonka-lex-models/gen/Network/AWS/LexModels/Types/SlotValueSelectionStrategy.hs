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

import qualified Network.AWS.Prelude as Prelude

newtype SlotValueSelectionStrategy = SlotValueSelectionStrategy'
  { fromSlotValueSelectionStrategy ::
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

pattern SlotValueSelectionStrategy_ORIGINAL_VALUE :: SlotValueSelectionStrategy
pattern SlotValueSelectionStrategy_ORIGINAL_VALUE = SlotValueSelectionStrategy' "ORIGINAL_VALUE"

pattern SlotValueSelectionStrategy_TOP_RESOLUTION :: SlotValueSelectionStrategy
pattern SlotValueSelectionStrategy_TOP_RESOLUTION = SlotValueSelectionStrategy' "TOP_RESOLUTION"

{-# COMPLETE
  SlotValueSelectionStrategy_ORIGINAL_VALUE,
  SlotValueSelectionStrategy_TOP_RESOLUTION,
  SlotValueSelectionStrategy'
  #-}
