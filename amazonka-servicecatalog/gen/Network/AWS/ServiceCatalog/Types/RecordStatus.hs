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
-- Module      : Network.AWS.ServiceCatalog.Types.RecordStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.RecordStatus
  ( RecordStatus
      ( ..,
        RecordStatus_CREATED,
        RecordStatus_FAILED,
        RecordStatus_IN_PROGRESS,
        RecordStatus_IN_PROGRESS_IN_ERROR,
        RecordStatus_SUCCEEDED
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype RecordStatus = RecordStatus'
  { fromRecordStatus ::
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

pattern RecordStatus_CREATED :: RecordStatus
pattern RecordStatus_CREATED = RecordStatus' "CREATED"

pattern RecordStatus_FAILED :: RecordStatus
pattern RecordStatus_FAILED = RecordStatus' "FAILED"

pattern RecordStatus_IN_PROGRESS :: RecordStatus
pattern RecordStatus_IN_PROGRESS = RecordStatus' "IN_PROGRESS"

pattern RecordStatus_IN_PROGRESS_IN_ERROR :: RecordStatus
pattern RecordStatus_IN_PROGRESS_IN_ERROR = RecordStatus' "IN_PROGRESS_IN_ERROR"

pattern RecordStatus_SUCCEEDED :: RecordStatus
pattern RecordStatus_SUCCEEDED = RecordStatus' "SUCCEEDED"

{-# COMPLETE
  RecordStatus_CREATED,
  RecordStatus_FAILED,
  RecordStatus_IN_PROGRESS,
  RecordStatus_IN_PROGRESS_IN_ERROR,
  RecordStatus_SUCCEEDED,
  RecordStatus'
  #-}
