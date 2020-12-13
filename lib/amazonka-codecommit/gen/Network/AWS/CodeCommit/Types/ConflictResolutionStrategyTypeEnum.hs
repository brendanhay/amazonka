{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.ConflictResolutionStrategyTypeEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.ConflictResolutionStrategyTypeEnum
  ( ConflictResolutionStrategyTypeEnum
      ( ConflictResolutionStrategyTypeEnum',
        None,
        AcceptSource,
        AcceptDestination,
        Automerge
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ConflictResolutionStrategyTypeEnum = ConflictResolutionStrategyTypeEnum' Lude.Text
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

pattern None :: ConflictResolutionStrategyTypeEnum
pattern None = ConflictResolutionStrategyTypeEnum' "NONE"

pattern AcceptSource :: ConflictResolutionStrategyTypeEnum
pattern AcceptSource = ConflictResolutionStrategyTypeEnum' "ACCEPT_SOURCE"

pattern AcceptDestination :: ConflictResolutionStrategyTypeEnum
pattern AcceptDestination = ConflictResolutionStrategyTypeEnum' "ACCEPT_DESTINATION"

pattern Automerge :: ConflictResolutionStrategyTypeEnum
pattern Automerge = ConflictResolutionStrategyTypeEnum' "AUTOMERGE"

{-# COMPLETE
  None,
  AcceptSource,
  AcceptDestination,
  Automerge,
  ConflictResolutionStrategyTypeEnum'
  #-}
