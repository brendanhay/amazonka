-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ConversionTaskState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ConversionTaskState
  ( ConversionTaskState
      ( ConversionTaskState',
        CTSActive,
        CTSCancelled,
        CTSCancelling,
        CTSCompleted
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ConversionTaskState = ConversionTaskState' Lude.Text
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

pattern CTSActive :: ConversionTaskState
pattern CTSActive = ConversionTaskState' "active"

pattern CTSCancelled :: ConversionTaskState
pattern CTSCancelled = ConversionTaskState' "cancelled"

pattern CTSCancelling :: ConversionTaskState
pattern CTSCancelling = ConversionTaskState' "cancelling"

pattern CTSCompleted :: ConversionTaskState
pattern CTSCompleted = ConversionTaskState' "completed"

{-# COMPLETE
  CTSActive,
  CTSCancelled,
  CTSCancelling,
  CTSCompleted,
  ConversionTaskState'
  #-}
