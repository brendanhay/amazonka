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
-- Module      : Amazonka.SecurityHub.Types.StandardsStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.StandardsStatus
  ( StandardsStatus
      ( ..,
        StandardsStatus_DELETING,
        StandardsStatus_FAILED,
        StandardsStatus_INCOMPLETE,
        StandardsStatus_PENDING,
        StandardsStatus_READY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype StandardsStatus = StandardsStatus'
  { fromStandardsStatus ::
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

pattern StandardsStatus_DELETING :: StandardsStatus
pattern StandardsStatus_DELETING = StandardsStatus' "DELETING"

pattern StandardsStatus_FAILED :: StandardsStatus
pattern StandardsStatus_FAILED = StandardsStatus' "FAILED"

pattern StandardsStatus_INCOMPLETE :: StandardsStatus
pattern StandardsStatus_INCOMPLETE = StandardsStatus' "INCOMPLETE"

pattern StandardsStatus_PENDING :: StandardsStatus
pattern StandardsStatus_PENDING = StandardsStatus' "PENDING"

pattern StandardsStatus_READY :: StandardsStatus
pattern StandardsStatus_READY = StandardsStatus' "READY"

{-# COMPLETE
  StandardsStatus_DELETING,
  StandardsStatus_FAILED,
  StandardsStatus_INCOMPLETE,
  StandardsStatus_PENDING,
  StandardsStatus_READY,
  StandardsStatus'
  #-}
