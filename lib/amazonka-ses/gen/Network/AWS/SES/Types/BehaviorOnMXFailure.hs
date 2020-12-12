{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.BehaviorOnMXFailure
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.BehaviorOnMXFailure
  ( BehaviorOnMXFailure
      ( BehaviorOnMXFailure',
        RejectMessage,
        UseDefaultValue
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype BehaviorOnMXFailure = BehaviorOnMXFailure' Lude.Text
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

pattern RejectMessage :: BehaviorOnMXFailure
pattern RejectMessage = BehaviorOnMXFailure' "RejectMessage"

pattern UseDefaultValue :: BehaviorOnMXFailure
pattern UseDefaultValue = BehaviorOnMXFailure' "UseDefaultValue"

{-# COMPLETE
  RejectMessage,
  UseDefaultValue,
  BehaviorOnMXFailure'
  #-}
