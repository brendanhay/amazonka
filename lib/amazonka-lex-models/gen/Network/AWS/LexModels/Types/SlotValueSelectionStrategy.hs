-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.SlotValueSelectionStrategy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.SlotValueSelectionStrategy
  ( SlotValueSelectionStrategy
      ( SlotValueSelectionStrategy',
        OriginalValue,
        TopResolution
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype SlotValueSelectionStrategy = SlotValueSelectionStrategy' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern OriginalValue :: SlotValueSelectionStrategy
pattern OriginalValue = SlotValueSelectionStrategy' "ORIGINAL_VALUE"

pattern TopResolution :: SlotValueSelectionStrategy
pattern TopResolution = SlotValueSelectionStrategy' "TOP_RESOLUTION"

{-# COMPLETE
  OriginalValue,
  TopResolution,
  SlotValueSelectionStrategy'
  #-}
