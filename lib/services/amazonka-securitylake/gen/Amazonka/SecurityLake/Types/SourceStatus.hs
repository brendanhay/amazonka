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
-- Module      : Amazonka.SecurityLake.Types.SourceStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityLake.Types.SourceStatus
  ( SourceStatus
      ( ..,
        SourceStatus_ACTIVE,
        SourceStatus_DEACTIVATED,
        SourceStatus_PENDING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SourceStatus = SourceStatus'
  { fromSourceStatus ::
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

pattern SourceStatus_ACTIVE :: SourceStatus
pattern SourceStatus_ACTIVE = SourceStatus' "ACTIVE"

pattern SourceStatus_DEACTIVATED :: SourceStatus
pattern SourceStatus_DEACTIVATED = SourceStatus' "DEACTIVATED"

pattern SourceStatus_PENDING :: SourceStatus
pattern SourceStatus_PENDING = SourceStatus' "PENDING"

{-# COMPLETE
  SourceStatus_ACTIVE,
  SourceStatus_DEACTIVATED,
  SourceStatus_PENDING,
  SourceStatus'
  #-}
