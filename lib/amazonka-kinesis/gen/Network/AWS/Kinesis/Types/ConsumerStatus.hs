-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.ConsumerStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types.ConsumerStatus
  ( ConsumerStatus
      ( ConsumerStatus',
        Active,
        Creating,
        Deleting
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ConsumerStatus = ConsumerStatus' Lude.Text
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

pattern Active :: ConsumerStatus
pattern Active = ConsumerStatus' "ACTIVE"

pattern Creating :: ConsumerStatus
pattern Creating = ConsumerStatus' "CREATING"

pattern Deleting :: ConsumerStatus
pattern Deleting = ConsumerStatus' "DELETING"

{-# COMPLETE
  Active,
  Creating,
  Deleting,
  ConsumerStatus'
  #-}
