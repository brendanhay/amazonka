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
-- Module      : Amazonka.CloudTrail.Types.ReadWriteType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudTrail.Types.ReadWriteType
  ( ReadWriteType
      ( ..,
        ReadWriteType_All,
        ReadWriteType_ReadOnly,
        ReadWriteType_WriteOnly
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ReadWriteType = ReadWriteType'
  { fromReadWriteType ::
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

pattern ReadWriteType_All :: ReadWriteType
pattern ReadWriteType_All = ReadWriteType' "All"

pattern ReadWriteType_ReadOnly :: ReadWriteType
pattern ReadWriteType_ReadOnly = ReadWriteType' "ReadOnly"

pattern ReadWriteType_WriteOnly :: ReadWriteType
pattern ReadWriteType_WriteOnly = ReadWriteType' "WriteOnly"

{-# COMPLETE
  ReadWriteType_All,
  ReadWriteType_ReadOnly,
  ReadWriteType_WriteOnly,
  ReadWriteType'
  #-}
