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
-- Module      : Network.AWS.IAM.Types.ReportStateType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.ReportStateType
  ( ReportStateType
      ( ..,
        ReportStateType_COMPLETE,
        ReportStateType_INPROGRESS,
        ReportStateType_STARTED
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ReportStateType = ReportStateType'
  { fromReportStateType ::
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
