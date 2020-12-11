-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.PullRequestStatusEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.PullRequestStatusEnum
  ( PullRequestStatusEnum
      ( PullRequestStatusEnum',
        Closed,
        Open
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype PullRequestStatusEnum = PullRequestStatusEnum' Lude.Text
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

pattern Closed :: PullRequestStatusEnum
pattern Closed = PullRequestStatusEnum' "CLOSED"

pattern Open :: PullRequestStatusEnum
pattern Open = PullRequestStatusEnum' "OPEN"

{-# COMPLETE
  Closed,
  Open,
  PullRequestStatusEnum'
  #-}
