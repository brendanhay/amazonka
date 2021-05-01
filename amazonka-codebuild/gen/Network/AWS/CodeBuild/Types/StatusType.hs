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
-- Module      : Network.AWS.CodeBuild.Types.StatusType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.StatusType
  ( StatusType
      ( ..,
        StatusType_FAILED,
        StatusType_FAULT,
        StatusType_IN_PROGRESS,
        StatusType_STOPPED,
        StatusType_SUCCEEDED,
        StatusType_TIMED_OUT
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype StatusType = StatusType'
  { fromStatusType ::
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

pattern StatusType_FAILED :: StatusType
pattern StatusType_FAILED = StatusType' "FAILED"

pattern StatusType_FAULT :: StatusType
pattern StatusType_FAULT = StatusType' "FAULT"

pattern StatusType_IN_PROGRESS :: StatusType
pattern StatusType_IN_PROGRESS = StatusType' "IN_PROGRESS"

pattern StatusType_STOPPED :: StatusType
pattern StatusType_STOPPED = StatusType' "STOPPED"

pattern StatusType_SUCCEEDED :: StatusType
pattern StatusType_SUCCEEDED = StatusType' "SUCCEEDED"

pattern StatusType_TIMED_OUT :: StatusType
pattern StatusType_TIMED_OUT = StatusType' "TIMED_OUT"

{-# COMPLETE
  StatusType_FAILED,
  StatusType_FAULT,
  StatusType_IN_PROGRESS,
  StatusType_STOPPED,
  StatusType_SUCCEEDED,
  StatusType_TIMED_OUT,
  StatusType'
  #-}
