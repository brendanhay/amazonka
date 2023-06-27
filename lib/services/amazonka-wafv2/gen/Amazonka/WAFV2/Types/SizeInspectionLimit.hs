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
-- Module      : Amazonka.WAFV2.Types.SizeInspectionLimit
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.SizeInspectionLimit
  ( SizeInspectionLimit
      ( ..,
        SizeInspectionLimit_KB_16,
        SizeInspectionLimit_KB_32,
        SizeInspectionLimit_KB_48,
        SizeInspectionLimit_KB_64
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SizeInspectionLimit = SizeInspectionLimit'
  { fromSizeInspectionLimit ::
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

pattern SizeInspectionLimit_KB_16 :: SizeInspectionLimit
pattern SizeInspectionLimit_KB_16 = SizeInspectionLimit' "KB_16"

pattern SizeInspectionLimit_KB_32 :: SizeInspectionLimit
pattern SizeInspectionLimit_KB_32 = SizeInspectionLimit' "KB_32"

pattern SizeInspectionLimit_KB_48 :: SizeInspectionLimit
pattern SizeInspectionLimit_KB_48 = SizeInspectionLimit' "KB_48"

pattern SizeInspectionLimit_KB_64 :: SizeInspectionLimit
pattern SizeInspectionLimit_KB_64 = SizeInspectionLimit' "KB_64"

{-# COMPLETE
  SizeInspectionLimit_KB_16,
  SizeInspectionLimit_KB_32,
  SizeInspectionLimit_KB_48,
  SizeInspectionLimit_KB_64,
  SizeInspectionLimit'
  #-}
