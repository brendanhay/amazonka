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
-- Module      : Network.AWS.MediaConvert.Types.JobStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.JobStatus
  ( JobStatus
      ( ..,
        JobStatus_CANCELED,
        JobStatus_COMPLETE,
        JobStatus_ERROR,
        JobStatus_PROGRESSING,
        JobStatus_SUBMITTED
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | A job\'s status can be SUBMITTED, PROGRESSING, COMPLETE, CANCELED, or
-- ERROR.
newtype JobStatus = JobStatus'
  { fromJobStatus ::
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

pattern JobStatus_CANCELED :: JobStatus
pattern JobStatus_CANCELED = JobStatus' "CANCELED"

pattern JobStatus_COMPLETE :: JobStatus
pattern JobStatus_COMPLETE = JobStatus' "COMPLETE"

pattern JobStatus_ERROR :: JobStatus
pattern JobStatus_ERROR = JobStatus' "ERROR"

pattern JobStatus_PROGRESSING :: JobStatus
pattern JobStatus_PROGRESSING = JobStatus' "PROGRESSING"

pattern JobStatus_SUBMITTED :: JobStatus
pattern JobStatus_SUBMITTED = JobStatus' "SUBMITTED"

{-# COMPLETE
  JobStatus_CANCELED,
  JobStatus_COMPLETE,
  JobStatus_ERROR,
  JobStatus_PROGRESSING,
  JobStatus_SUBMITTED,
  JobStatus'
  #-}
