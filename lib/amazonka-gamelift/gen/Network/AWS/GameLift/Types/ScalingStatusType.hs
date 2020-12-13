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
        SSTActive,
        SSTUpdateRequested,
        SSTUpdating,
        SSTDeleteRequested,
        SSTDeleting,
        SSTDeleted,
        SSTError
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

pattern SSTActive :: ScalingStatusType
pattern SSTActive = ScalingStatusType' "ACTIVE"

pattern SSTUpdateRequested :: ScalingStatusType
pattern SSTUpdateRequested = ScalingStatusType' "UPDATE_REQUESTED"

pattern SSTUpdating :: ScalingStatusType
pattern SSTUpdating = ScalingStatusType' "UPDATING"

pattern SSTDeleteRequested :: ScalingStatusType
pattern SSTDeleteRequested = ScalingStatusType' "DELETE_REQUESTED"

pattern SSTDeleting :: ScalingStatusType
pattern SSTDeleting = ScalingStatusType' "DELETING"

pattern SSTDeleted :: ScalingStatusType
pattern SSTDeleted = ScalingStatusType' "DELETED"

pattern SSTError :: ScalingStatusType
pattern SSTError = ScalingStatusType' "ERROR"

{-# COMPLETE
  SSTActive,
  SSTUpdateRequested,
  SSTUpdating,
  SSTDeleteRequested,
  SSTDeleting,
  SSTDeleted,
  SSTError,
  ScalingStatusType'
  #-}
