{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.ScalingStatusType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.ScalingStatusType
  ( ScalingStatusType
      ( ScalingStatusType',
        Active,
        DeleteRequested,
        Deleted,
        Deleting,
        Error,
        UpdateRequested,
        Updating
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ScalingStatusType = ScalingStatusType' Lude.Text
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

pattern Active :: ScalingStatusType
pattern Active = ScalingStatusType' "ACTIVE"

pattern DeleteRequested :: ScalingStatusType
pattern DeleteRequested = ScalingStatusType' "DELETE_REQUESTED"

pattern Deleted :: ScalingStatusType
pattern Deleted = ScalingStatusType' "DELETED"

pattern Deleting :: ScalingStatusType
pattern Deleting = ScalingStatusType' "DELETING"

pattern Error :: ScalingStatusType
pattern Error = ScalingStatusType' "ERROR"

pattern UpdateRequested :: ScalingStatusType
pattern UpdateRequested = ScalingStatusType' "UPDATE_REQUESTED"

pattern Updating :: ScalingStatusType
pattern Updating = ScalingStatusType' "UPDATING"

{-# COMPLETE
  Active,
  DeleteRequested,
  Deleted,
  Deleting,
  Error,
  UpdateRequested,
  Updating,
  ScalingStatusType'
  #-}
