{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.Types.BulkPublishStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoSync.Types.BulkPublishStatus
  ( BulkPublishStatus
      ( ..,
        BulkPublishStatus_FAILED,
        BulkPublishStatus_IN_PROGRESS,
        BulkPublishStatus_NOT_STARTED,
        BulkPublishStatus_SUCCEEDED
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype BulkPublishStatus = BulkPublishStatus'
  { fromBulkPublishStatus ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern BulkPublishStatus_FAILED :: BulkPublishStatus
pattern BulkPublishStatus_FAILED = BulkPublishStatus' "FAILED"

pattern BulkPublishStatus_IN_PROGRESS :: BulkPublishStatus
pattern BulkPublishStatus_IN_PROGRESS = BulkPublishStatus' "IN_PROGRESS"

pattern BulkPublishStatus_NOT_STARTED :: BulkPublishStatus
pattern BulkPublishStatus_NOT_STARTED = BulkPublishStatus' "NOT_STARTED"

pattern BulkPublishStatus_SUCCEEDED :: BulkPublishStatus
pattern BulkPublishStatus_SUCCEEDED = BulkPublishStatus' "SUCCEEDED"

{-# COMPLETE
  BulkPublishStatus_FAILED,
  BulkPublishStatus_IN_PROGRESS,
  BulkPublishStatus_NOT_STARTED,
  BulkPublishStatus_SUCCEEDED,
  BulkPublishStatus'
  #-}
