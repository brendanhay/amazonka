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
-- Module      : Amazonka.Redshift.Types.TableRestoreStatusType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.TableRestoreStatusType
  ( TableRestoreStatusType
      ( ..,
        TableRestoreStatusType_CANCELED,
        TableRestoreStatusType_FAILED,
        TableRestoreStatusType_IN_PROGRESS,
        TableRestoreStatusType_PENDING,
        TableRestoreStatusType_SUCCEEDED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal

newtype TableRestoreStatusType = TableRestoreStatusType'
  { fromTableRestoreStatusType ::
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

pattern TableRestoreStatusType_CANCELED :: TableRestoreStatusType
pattern TableRestoreStatusType_CANCELED = TableRestoreStatusType' "CANCELED"

pattern TableRestoreStatusType_FAILED :: TableRestoreStatusType
pattern TableRestoreStatusType_FAILED = TableRestoreStatusType' "FAILED"

pattern TableRestoreStatusType_IN_PROGRESS :: TableRestoreStatusType
pattern TableRestoreStatusType_IN_PROGRESS = TableRestoreStatusType' "IN_PROGRESS"

pattern TableRestoreStatusType_PENDING :: TableRestoreStatusType
pattern TableRestoreStatusType_PENDING = TableRestoreStatusType' "PENDING"

pattern TableRestoreStatusType_SUCCEEDED :: TableRestoreStatusType
pattern TableRestoreStatusType_SUCCEEDED = TableRestoreStatusType' "SUCCEEDED"

{-# COMPLETE
  TableRestoreStatusType_CANCELED,
  TableRestoreStatusType_FAILED,
  TableRestoreStatusType_IN_PROGRESS,
  TableRestoreStatusType_PENDING,
  TableRestoreStatusType_SUCCEEDED,
  TableRestoreStatusType'
  #-}
