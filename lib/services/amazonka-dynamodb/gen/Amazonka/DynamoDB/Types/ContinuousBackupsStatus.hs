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
-- Module      : Amazonka.DynamoDB.Types.ContinuousBackupsStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.ContinuousBackupsStatus
  ( ContinuousBackupsStatus
      ( ..,
        ContinuousBackupsStatus_DISABLED,
        ContinuousBackupsStatus_ENABLED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.TransactWriteItem
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

newtype ContinuousBackupsStatus = ContinuousBackupsStatus'
  { fromContinuousBackupsStatus ::
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

pattern ContinuousBackupsStatus_DISABLED :: ContinuousBackupsStatus
pattern ContinuousBackupsStatus_DISABLED = ContinuousBackupsStatus' "DISABLED"

pattern ContinuousBackupsStatus_ENABLED :: ContinuousBackupsStatus
pattern ContinuousBackupsStatus_ENABLED = ContinuousBackupsStatus' "ENABLED"

{-# COMPLETE
  ContinuousBackupsStatus_DISABLED,
  ContinuousBackupsStatus_ENABLED,
  ContinuousBackupsStatus'
  #-}
