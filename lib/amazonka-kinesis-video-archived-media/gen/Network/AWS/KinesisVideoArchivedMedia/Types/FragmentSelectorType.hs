-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types.FragmentSelectorType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideoArchivedMedia.Types.FragmentSelectorType
  ( FragmentSelectorType
      ( FragmentSelectorType',
        FSTProducerTimestamp,
        FSTServerTimestamp
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype FragmentSelectorType = FragmentSelectorType' Lude.Text
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

pattern FSTProducerTimestamp :: FragmentSelectorType
pattern FSTProducerTimestamp = FragmentSelectorType' "PRODUCER_TIMESTAMP"

pattern FSTServerTimestamp :: FragmentSelectorType
pattern FSTServerTimestamp = FragmentSelectorType' "SERVER_TIMESTAMP"

{-# COMPLETE
  FSTProducerTimestamp,
  FSTServerTimestamp,
  FragmentSelectorType'
  #-}
