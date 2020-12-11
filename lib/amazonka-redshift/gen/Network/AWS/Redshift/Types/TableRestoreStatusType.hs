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
        TRSTCanceled,
        TRSTFailed,
        TRSTInProgress,
        TRSTPending,
        TRSTSucceeded
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

pattern TRSTCanceled :: TableRestoreStatusType
pattern TRSTCanceled = TableRestoreStatusType' "CANCELED"

pattern TRSTFailed :: TableRestoreStatusType
pattern TRSTFailed = TableRestoreStatusType' "FAILED"

pattern TRSTInProgress :: TableRestoreStatusType
pattern TRSTInProgress = TableRestoreStatusType' "IN_PROGRESS"

pattern TRSTPending :: TableRestoreStatusType
pattern TRSTPending = TableRestoreStatusType' "PENDING"

pattern TRSTSucceeded :: TableRestoreStatusType
pattern TRSTSucceeded = TableRestoreStatusType' "SUCCEEDED"

{-# COMPLETE
  TRSTCanceled,
  TRSTFailed,
  TRSTInProgress,
  TRSTPending,
  TRSTSucceeded,
  TableRestoreStatusType'
  #-}
