{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoMedia.Types.StartSelectorType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideoMedia.Types.StartSelectorType
  ( StartSelectorType
      ( StartSelectorType',
        FragmentNumber,
        ServerTimestamp,
        ProducerTimestamp,
        Now,
        Earliest,
        ContinuationToken
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype StartSelectorType = StartSelectorType' Lude.Text
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

pattern FragmentNumber :: StartSelectorType
pattern FragmentNumber = StartSelectorType' "FRAGMENT_NUMBER"

pattern ServerTimestamp :: StartSelectorType
pattern ServerTimestamp = StartSelectorType' "SERVER_TIMESTAMP"

pattern ProducerTimestamp :: StartSelectorType
pattern ProducerTimestamp = StartSelectorType' "PRODUCER_TIMESTAMP"

pattern Now :: StartSelectorType
pattern Now = StartSelectorType' "NOW"

pattern Earliest :: StartSelectorType
pattern Earliest = StartSelectorType' "EARLIEST"

pattern ContinuationToken :: StartSelectorType
pattern ContinuationToken = StartSelectorType' "CONTINUATION_TOKEN"

{-# COMPLETE
  FragmentNumber,
  ServerTimestamp,
  ProducerTimestamp,
  Now,
  Earliest,
  ContinuationToken,
  StartSelectorType'
  #-}
