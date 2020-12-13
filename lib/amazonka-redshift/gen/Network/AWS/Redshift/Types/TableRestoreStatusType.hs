{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.TableRestoreStatusType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.TableRestoreStatusType
  ( TableRestoreStatusType
      ( TableRestoreStatusType',
        Pending,
        InProgress,
        Succeeded,
        Failed,
        Canceled
      ),
  )
where

import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal

newtype TableRestoreStatusType = TableRestoreStatusType' Lude.Text
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

pattern Pending :: TableRestoreStatusType
pattern Pending = TableRestoreStatusType' "PENDING"

pattern InProgress :: TableRestoreStatusType
pattern InProgress = TableRestoreStatusType' "IN_PROGRESS"

pattern Succeeded :: TableRestoreStatusType
pattern Succeeded = TableRestoreStatusType' "SUCCEEDED"

pattern Failed :: TableRestoreStatusType
pattern Failed = TableRestoreStatusType' "FAILED"

pattern Canceled :: TableRestoreStatusType
pattern Canceled = TableRestoreStatusType' "CANCELED"

{-# COMPLETE
  Pending,
  InProgress,
  Succeeded,
  Failed,
  Canceled,
  TableRestoreStatusType'
  #-}
