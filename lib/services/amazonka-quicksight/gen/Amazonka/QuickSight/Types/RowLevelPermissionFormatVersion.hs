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
-- Module      : Amazonka.QuickSight.Types.RowLevelPermissionFormatVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.RowLevelPermissionFormatVersion
  ( RowLevelPermissionFormatVersion
      ( ..,
        RowLevelPermissionFormatVersion_VERSION_1,
        RowLevelPermissionFormatVersion_VERSION_2
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RowLevelPermissionFormatVersion = RowLevelPermissionFormatVersion'
  { fromRowLevelPermissionFormatVersion ::
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

pattern RowLevelPermissionFormatVersion_VERSION_1 :: RowLevelPermissionFormatVersion
pattern RowLevelPermissionFormatVersion_VERSION_1 = RowLevelPermissionFormatVersion' "VERSION_1"

pattern RowLevelPermissionFormatVersion_VERSION_2 :: RowLevelPermissionFormatVersion
pattern RowLevelPermissionFormatVersion_VERSION_2 = RowLevelPermissionFormatVersion' "VERSION_2"

{-# COMPLETE
  RowLevelPermissionFormatVersion_VERSION_1,
  RowLevelPermissionFormatVersion_VERSION_2,
  RowLevelPermissionFormatVersion'
  #-}
