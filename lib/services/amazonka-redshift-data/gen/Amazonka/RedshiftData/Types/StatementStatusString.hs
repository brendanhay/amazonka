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
-- Module      : Amazonka.RedshiftData.Types.StatementStatusString
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RedshiftData.Types.StatementStatusString
  ( StatementStatusString
      ( ..,
        StatementStatusString_ABORTED,
        StatementStatusString_FAILED,
        StatementStatusString_FINISHED,
        StatementStatusString_PICKED,
        StatementStatusString_STARTED,
        StatementStatusString_SUBMITTED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype StatementStatusString = StatementStatusString'
  { fromStatementStatusString ::
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

pattern StatementStatusString_ABORTED :: StatementStatusString
pattern StatementStatusString_ABORTED = StatementStatusString' "ABORTED"

pattern StatementStatusString_FAILED :: StatementStatusString
pattern StatementStatusString_FAILED = StatementStatusString' "FAILED"

pattern StatementStatusString_FINISHED :: StatementStatusString
pattern StatementStatusString_FINISHED = StatementStatusString' "FINISHED"

pattern StatementStatusString_PICKED :: StatementStatusString
pattern StatementStatusString_PICKED = StatementStatusString' "PICKED"

pattern StatementStatusString_STARTED :: StatementStatusString
pattern StatementStatusString_STARTED = StatementStatusString' "STARTED"

pattern StatementStatusString_SUBMITTED :: StatementStatusString
pattern StatementStatusString_SUBMITTED = StatementStatusString' "SUBMITTED"

{-# COMPLETE
  StatementStatusString_ABORTED,
  StatementStatusString_FAILED,
  StatementStatusString_FINISHED,
  StatementStatusString_PICKED,
  StatementStatusString_STARTED,
  StatementStatusString_SUBMITTED,
  StatementStatusString'
  #-}
