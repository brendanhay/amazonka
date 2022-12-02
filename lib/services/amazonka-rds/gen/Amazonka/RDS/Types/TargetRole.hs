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
-- Module      : Amazonka.RDS.Types.TargetRole
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.TargetRole
  ( TargetRole
      ( ..,
        TargetRole_READ_ONLY,
        TargetRole_READ_WRITE,
        TargetRole_UNKNOWN
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TargetRole = TargetRole'
  { fromTargetRole ::
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

pattern TargetRole_READ_ONLY :: TargetRole
pattern TargetRole_READ_ONLY = TargetRole' "READ_ONLY"

pattern TargetRole_READ_WRITE :: TargetRole
pattern TargetRole_READ_WRITE = TargetRole' "READ_WRITE"

pattern TargetRole_UNKNOWN :: TargetRole
pattern TargetRole_UNKNOWN = TargetRole' "UNKNOWN"

{-# COMPLETE
  TargetRole_READ_ONLY,
  TargetRole_READ_WRITE,
  TargetRole_UNKNOWN,
  TargetRole'
  #-}
