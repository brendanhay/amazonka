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
-- Module      : Amazonka.Nimble.Types.StudioComponentType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.StudioComponentType
  ( StudioComponentType
      ( ..,
        StudioComponentType_ACTIVE_DIRECTORY,
        StudioComponentType_COMPUTE_FARM,
        StudioComponentType_CUSTOM,
        StudioComponentType_LICENSE_SERVICE,
        StudioComponentType_SHARED_FILE_SYSTEM
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype StudioComponentType = StudioComponentType'
  { fromStudioComponentType ::
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

pattern StudioComponentType_ACTIVE_DIRECTORY :: StudioComponentType
pattern StudioComponentType_ACTIVE_DIRECTORY = StudioComponentType' "ACTIVE_DIRECTORY"

pattern StudioComponentType_COMPUTE_FARM :: StudioComponentType
pattern StudioComponentType_COMPUTE_FARM = StudioComponentType' "COMPUTE_FARM"

pattern StudioComponentType_CUSTOM :: StudioComponentType
pattern StudioComponentType_CUSTOM = StudioComponentType' "CUSTOM"

pattern StudioComponentType_LICENSE_SERVICE :: StudioComponentType
pattern StudioComponentType_LICENSE_SERVICE = StudioComponentType' "LICENSE_SERVICE"

pattern StudioComponentType_SHARED_FILE_SYSTEM :: StudioComponentType
pattern StudioComponentType_SHARED_FILE_SYSTEM = StudioComponentType' "SHARED_FILE_SYSTEM"

{-# COMPLETE
  StudioComponentType_ACTIVE_DIRECTORY,
  StudioComponentType_COMPUTE_FARM,
  StudioComponentType_CUSTOM,
  StudioComponentType_LICENSE_SERVICE,
  StudioComponentType_SHARED_FILE_SYSTEM,
  StudioComponentType'
  #-}
