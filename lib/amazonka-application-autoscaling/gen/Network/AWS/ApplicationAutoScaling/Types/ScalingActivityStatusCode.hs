{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling.Types.ScalingActivityStatusCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ApplicationAutoScaling.Types.ScalingActivityStatusCode
  ( ScalingActivityStatusCode
      ( ScalingActivityStatusCode',
        Pending,
        InProgress,
        Successful,
        Overridden,
        Unfulfilled,
        Failed
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ScalingActivityStatusCode = ScalingActivityStatusCode' Lude.Text
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

pattern Pending :: ScalingActivityStatusCode
pattern Pending = ScalingActivityStatusCode' "Pending"

pattern InProgress :: ScalingActivityStatusCode
pattern InProgress = ScalingActivityStatusCode' "InProgress"

pattern Successful :: ScalingActivityStatusCode
pattern Successful = ScalingActivityStatusCode' "Successful"

pattern Overridden :: ScalingActivityStatusCode
pattern Overridden = ScalingActivityStatusCode' "Overridden"

pattern Unfulfilled :: ScalingActivityStatusCode
pattern Unfulfilled = ScalingActivityStatusCode' "Unfulfilled"

pattern Failed :: ScalingActivityStatusCode
pattern Failed = ScalingActivityStatusCode' "Failed"

{-# COMPLETE
  Pending,
  InProgress,
  Successful,
  Overridden,
  Unfulfilled,
  Failed,
  ScalingActivityStatusCode'
  #-}
