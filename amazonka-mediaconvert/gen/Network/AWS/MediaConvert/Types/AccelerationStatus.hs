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
-- Module      : Network.AWS.MediaConvert.Types.AccelerationStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AccelerationStatus
  ( AccelerationStatus
      ( ..,
        AccelerationStatus_ACCELERATED,
        AccelerationStatus_IN_PROGRESS,
        AccelerationStatus_NOT_ACCELERATED,
        AccelerationStatus_NOT_APPLICABLE
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Describes whether the current job is running with accelerated
-- transcoding. For jobs that have Acceleration (AccelerationMode) set to
-- DISABLED, AccelerationStatus is always NOT_APPLICABLE. For jobs that
-- have Acceleration (AccelerationMode) set to ENABLED or PREFERRED,
-- AccelerationStatus is one of the other states. AccelerationStatus is
-- IN_PROGRESS initially, while the service determines whether the input
-- files and job settings are compatible with accelerated transcoding. If
-- they are, AcclerationStatus is ACCELERATED. If your input files and job
-- settings aren\'t compatible with accelerated transcoding, the service
-- either fails your job or runs it without accelerated transcoding,
-- depending on how you set Acceleration (AccelerationMode). When the
-- service runs your job without accelerated transcoding,
-- AccelerationStatus is NOT_ACCELERATED.
newtype AccelerationStatus = AccelerationStatus'
  { fromAccelerationStatus ::
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

pattern AccelerationStatus_ACCELERATED :: AccelerationStatus
pattern AccelerationStatus_ACCELERATED = AccelerationStatus' "ACCELERATED"

pattern AccelerationStatus_IN_PROGRESS :: AccelerationStatus
pattern AccelerationStatus_IN_PROGRESS = AccelerationStatus' "IN_PROGRESS"

pattern AccelerationStatus_NOT_ACCELERATED :: AccelerationStatus
pattern AccelerationStatus_NOT_ACCELERATED = AccelerationStatus' "NOT_ACCELERATED"

pattern AccelerationStatus_NOT_APPLICABLE :: AccelerationStatus
pattern AccelerationStatus_NOT_APPLICABLE = AccelerationStatus' "NOT_APPLICABLE"

{-# COMPLETE
  AccelerationStatus_ACCELERATED,
  AccelerationStatus_IN_PROGRESS,
  AccelerationStatus_NOT_ACCELERATED,
  AccelerationStatus_NOT_APPLICABLE,
  AccelerationStatus'
  #-}
