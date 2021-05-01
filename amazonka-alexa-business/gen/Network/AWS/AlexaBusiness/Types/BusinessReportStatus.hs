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
-- Module      : Network.AWS.AlexaBusiness.Types.BusinessReportStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.BusinessReportStatus
  ( BusinessReportStatus
      ( ..,
        BusinessReportStatus_FAILED,
        BusinessReportStatus_RUNNING,
        BusinessReportStatus_SUCCEEDED
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype BusinessReportStatus = BusinessReportStatus'
  { fromBusinessReportStatus ::
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

pattern BusinessReportStatus_FAILED :: BusinessReportStatus
pattern BusinessReportStatus_FAILED = BusinessReportStatus' "FAILED"

pattern BusinessReportStatus_RUNNING :: BusinessReportStatus
pattern BusinessReportStatus_RUNNING = BusinessReportStatus' "RUNNING"

pattern BusinessReportStatus_SUCCEEDED :: BusinessReportStatus
pattern BusinessReportStatus_SUCCEEDED = BusinessReportStatus' "SUCCEEDED"

{-# COMPLETE
  BusinessReportStatus_FAILED,
  BusinessReportStatus_RUNNING,
  BusinessReportStatus_SUCCEEDED,
  BusinessReportStatus'
  #-}
