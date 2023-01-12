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
-- Module      : Amazonka.IAM.Types.ReportStateType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IAM.Types.ReportStateType
  ( ReportStateType
      ( ..,
        ReportStateType_COMPLETE,
        ReportStateType_INPROGRESS,
        ReportStateType_STARTED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ReportStateType = ReportStateType'
  { fromReportStateType ::
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

pattern ReportStateType_COMPLETE :: ReportStateType
pattern ReportStateType_COMPLETE = ReportStateType' "COMPLETE"

pattern ReportStateType_INPROGRESS :: ReportStateType
pattern ReportStateType_INPROGRESS = ReportStateType' "INPROGRESS"

pattern ReportStateType_STARTED :: ReportStateType
pattern ReportStateType_STARTED = ReportStateType' "STARTED"

{-# COMPLETE
  ReportStateType_COMPLETE,
  ReportStateType_INPROGRESS,
  ReportStateType_STARTED,
  ReportStateType'
  #-}
