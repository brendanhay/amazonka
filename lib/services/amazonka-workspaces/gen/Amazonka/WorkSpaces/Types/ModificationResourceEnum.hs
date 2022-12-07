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
-- Module      : Amazonka.WorkSpaces.Types.ModificationResourceEnum
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpaces.Types.ModificationResourceEnum
  ( ModificationResourceEnum
      ( ..,
        ModificationResourceEnum_COMPUTE_TYPE,
        ModificationResourceEnum_ROOT_VOLUME,
        ModificationResourceEnum_USER_VOLUME
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ModificationResourceEnum = ModificationResourceEnum'
  { fromModificationResourceEnum ::
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

pattern ModificationResourceEnum_COMPUTE_TYPE :: ModificationResourceEnum
pattern ModificationResourceEnum_COMPUTE_TYPE = ModificationResourceEnum' "COMPUTE_TYPE"

pattern ModificationResourceEnum_ROOT_VOLUME :: ModificationResourceEnum
pattern ModificationResourceEnum_ROOT_VOLUME = ModificationResourceEnum' "ROOT_VOLUME"

pattern ModificationResourceEnum_USER_VOLUME :: ModificationResourceEnum
pattern ModificationResourceEnum_USER_VOLUME = ModificationResourceEnum' "USER_VOLUME"

{-# COMPLETE
  ModificationResourceEnum_COMPUTE_TYPE,
  ModificationResourceEnum_ROOT_VOLUME,
  ModificationResourceEnum_USER_VOLUME,
  ModificationResourceEnum'
  #-}
