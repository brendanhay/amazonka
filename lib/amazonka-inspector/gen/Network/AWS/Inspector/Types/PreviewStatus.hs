-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.PreviewStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.PreviewStatus
  ( PreviewStatus
      ( PreviewStatus',
        PSCompleted,
        PSWorkInProgress
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype PreviewStatus = PreviewStatus' Lude.Text
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

pattern PSCompleted :: PreviewStatus
pattern PSCompleted = PreviewStatus' "COMPLETED"

pattern PSWorkInProgress :: PreviewStatus
pattern PSWorkInProgress = PreviewStatus' "WORK_IN_PROGRESS"

{-# COMPLETE
  PSCompleted,
  PSWorkInProgress,
  PreviewStatus'
  #-}
