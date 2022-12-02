{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ChimeSdkMeetings.Types.MeetingFeatureStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMeetings.Types.MeetingFeatureStatus
  ( MeetingFeatureStatus
      ( ..,
        MeetingFeatureStatus_AVAILABLE,
        MeetingFeatureStatus_UNAVAILABLE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype MeetingFeatureStatus = MeetingFeatureStatus'
  { fromMeetingFeatureStatus ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern MeetingFeatureStatus_AVAILABLE :: MeetingFeatureStatus
pattern MeetingFeatureStatus_AVAILABLE = MeetingFeatureStatus' "AVAILABLE"

pattern MeetingFeatureStatus_UNAVAILABLE :: MeetingFeatureStatus
pattern MeetingFeatureStatus_UNAVAILABLE = MeetingFeatureStatus' "UNAVAILABLE"

{-# COMPLETE
  MeetingFeatureStatus_AVAILABLE,
  MeetingFeatureStatus_UNAVAILABLE,
  MeetingFeatureStatus'
  #-}
