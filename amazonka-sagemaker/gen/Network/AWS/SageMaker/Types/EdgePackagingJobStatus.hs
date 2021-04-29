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
-- Module      : Network.AWS.SageMaker.Types.EdgePackagingJobStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.EdgePackagingJobStatus
  ( EdgePackagingJobStatus
      ( ..,
        EdgePackagingJobStatus_COMPLETED,
        EdgePackagingJobStatus_FAILED,
        EdgePackagingJobStatus_INPROGRESS,
        EdgePackagingJobStatus_STARTING,
        EdgePackagingJobStatus_STOPPED,
        EdgePackagingJobStatus_STOPPING
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype EdgePackagingJobStatus = EdgePackagingJobStatus'
  { fromEdgePackagingJobStatus ::
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

pattern EdgePackagingJobStatus_COMPLETED :: EdgePackagingJobStatus
pattern EdgePackagingJobStatus_COMPLETED = EdgePackagingJobStatus' "COMPLETED"

pattern EdgePackagingJobStatus_FAILED :: EdgePackagingJobStatus
pattern EdgePackagingJobStatus_FAILED = EdgePackagingJobStatus' "FAILED"

pattern EdgePackagingJobStatus_INPROGRESS :: EdgePackagingJobStatus
pattern EdgePackagingJobStatus_INPROGRESS = EdgePackagingJobStatus' "INPROGRESS"

pattern EdgePackagingJobStatus_STARTING :: EdgePackagingJobStatus
pattern EdgePackagingJobStatus_STARTING = EdgePackagingJobStatus' "STARTING"

pattern EdgePackagingJobStatus_STOPPED :: EdgePackagingJobStatus
pattern EdgePackagingJobStatus_STOPPED = EdgePackagingJobStatus' "STOPPED"

pattern EdgePackagingJobStatus_STOPPING :: EdgePackagingJobStatus
pattern EdgePackagingJobStatus_STOPPING = EdgePackagingJobStatus' "STOPPING"

{-# COMPLETE
  EdgePackagingJobStatus_COMPLETED,
  EdgePackagingJobStatus_FAILED,
  EdgePackagingJobStatus_INPROGRESS,
  EdgePackagingJobStatus_STARTING,
  EdgePackagingJobStatus_STOPPED,
  EdgePackagingJobStatus_STOPPING,
  EdgePackagingJobStatus'
  #-}
