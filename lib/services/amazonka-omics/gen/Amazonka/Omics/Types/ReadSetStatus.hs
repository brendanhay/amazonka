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
-- Module      : Amazonka.Omics.Types.ReadSetStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.ReadSetStatus
  ( ReadSetStatus
      ( ..,
        ReadSetStatus_ACTIVATING,
        ReadSetStatus_ACTIVE,
        ReadSetStatus_ARCHIVED,
        ReadSetStatus_DELETED,
        ReadSetStatus_DELETING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ReadSetStatus = ReadSetStatus'
  { fromReadSetStatus ::
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

pattern ReadSetStatus_ACTIVATING :: ReadSetStatus
pattern ReadSetStatus_ACTIVATING = ReadSetStatus' "ACTIVATING"

pattern ReadSetStatus_ACTIVE :: ReadSetStatus
pattern ReadSetStatus_ACTIVE = ReadSetStatus' "ACTIVE"

pattern ReadSetStatus_ARCHIVED :: ReadSetStatus
pattern ReadSetStatus_ARCHIVED = ReadSetStatus' "ARCHIVED"

pattern ReadSetStatus_DELETED :: ReadSetStatus
pattern ReadSetStatus_DELETED = ReadSetStatus' "DELETED"

pattern ReadSetStatus_DELETING :: ReadSetStatus
pattern ReadSetStatus_DELETING = ReadSetStatus' "DELETING"

{-# COMPLETE
  ReadSetStatus_ACTIVATING,
  ReadSetStatus_ACTIVE,
  ReadSetStatus_ARCHIVED,
  ReadSetStatus_DELETED,
  ReadSetStatus_DELETING,
  ReadSetStatus'
  #-}
