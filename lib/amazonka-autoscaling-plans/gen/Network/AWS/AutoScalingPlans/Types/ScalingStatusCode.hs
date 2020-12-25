{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.ScalingStatusCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScalingPlans.Types.ScalingStatusCode
  ( ScalingStatusCode
      ( ScalingStatusCode',
        ScalingStatusCodeInactive,
        ScalingStatusCodePartiallyActive,
        ScalingStatusCodeActive,
        fromScalingStatusCode
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ScalingStatusCode = ScalingStatusCode'
  { fromScalingStatusCode ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern ScalingStatusCodeInactive :: ScalingStatusCode
pattern ScalingStatusCodeInactive = ScalingStatusCode' "Inactive"

pattern ScalingStatusCodePartiallyActive :: ScalingStatusCode
pattern ScalingStatusCodePartiallyActive = ScalingStatusCode' "PartiallyActive"

pattern ScalingStatusCodeActive :: ScalingStatusCode
pattern ScalingStatusCodeActive = ScalingStatusCode' "Active"

{-# COMPLETE
  ScalingStatusCodeInactive,
  ScalingStatusCodePartiallyActive,
  ScalingStatusCodeActive,
  ScalingStatusCode'
  #-}
