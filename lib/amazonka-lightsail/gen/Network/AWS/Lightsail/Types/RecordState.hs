-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.RecordState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.RecordState
  ( RecordState
      ( RecordState',
        RFailed,
        RStarted,
        RSucceeded
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype RecordState = RecordState' Lude.Text
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

pattern RFailed :: RecordState
pattern RFailed = RecordState' "Failed"

pattern RStarted :: RecordState
pattern RStarted = RecordState' "Started"

pattern RSucceeded :: RecordState
pattern RSucceeded = RecordState' "Succeeded"

{-# COMPLETE
  RFailed,
  RStarted,
  RSucceeded,
  RecordState'
  #-}
