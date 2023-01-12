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
-- Module      : Amazonka.Redshift.Types.DataShareStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.DataShareStatus
  ( DataShareStatus
      ( ..,
        DataShareStatus_ACTIVE,
        DataShareStatus_AUTHORIZED,
        DataShareStatus_AVAILABLE,
        DataShareStatus_DEAUTHORIZED,
        DataShareStatus_PENDING_AUTHORIZATION,
        DataShareStatus_REJECTED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal

newtype DataShareStatus = DataShareStatus'
  { fromDataShareStatus ::
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

pattern DataShareStatus_ACTIVE :: DataShareStatus
pattern DataShareStatus_ACTIVE = DataShareStatus' "ACTIVE"

pattern DataShareStatus_AUTHORIZED :: DataShareStatus
pattern DataShareStatus_AUTHORIZED = DataShareStatus' "AUTHORIZED"

pattern DataShareStatus_AVAILABLE :: DataShareStatus
pattern DataShareStatus_AVAILABLE = DataShareStatus' "AVAILABLE"

pattern DataShareStatus_DEAUTHORIZED :: DataShareStatus
pattern DataShareStatus_DEAUTHORIZED = DataShareStatus' "DEAUTHORIZED"

pattern DataShareStatus_PENDING_AUTHORIZATION :: DataShareStatus
pattern DataShareStatus_PENDING_AUTHORIZATION = DataShareStatus' "PENDING_AUTHORIZATION"

pattern DataShareStatus_REJECTED :: DataShareStatus
pattern DataShareStatus_REJECTED = DataShareStatus' "REJECTED"

{-# COMPLETE
  DataShareStatus_ACTIVE,
  DataShareStatus_AUTHORIZED,
  DataShareStatus_AVAILABLE,
  DataShareStatus_DEAUTHORIZED,
  DataShareStatus_PENDING_AUTHORIZATION,
  DataShareStatus_REJECTED,
  DataShareStatus'
  #-}
