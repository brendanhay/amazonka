-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.BucketLogsPermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.BucketLogsPermission
  ( BucketLogsPermission
      ( BucketLogsPermission',
        FullControl,
        Read,
        Write
      ),
  )
where

import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal

newtype BucketLogsPermission = BucketLogsPermission' Lude.Text
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

pattern FullControl :: BucketLogsPermission
pattern FullControl = BucketLogsPermission' "FULL_CONTROL"

pattern Read :: BucketLogsPermission
pattern Read = BucketLogsPermission' "READ"

pattern Write :: BucketLogsPermission
pattern Write = BucketLogsPermission' "WRITE"

{-# COMPLETE
  FullControl,
  Read,
  Write,
  BucketLogsPermission'
  #-}
