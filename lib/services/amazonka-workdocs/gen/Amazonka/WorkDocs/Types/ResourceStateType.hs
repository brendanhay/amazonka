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
-- Module      : Amazonka.WorkDocs.Types.ResourceStateType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkDocs.Types.ResourceStateType
  ( ResourceStateType
      ( ..,
        ResourceStateType_ACTIVE,
        ResourceStateType_RECYCLED,
        ResourceStateType_RECYCLING,
        ResourceStateType_RESTORING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ResourceStateType = ResourceStateType'
  { fromResourceStateType ::
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

pattern ResourceStateType_ACTIVE :: ResourceStateType
pattern ResourceStateType_ACTIVE = ResourceStateType' "ACTIVE"

pattern ResourceStateType_RECYCLED :: ResourceStateType
pattern ResourceStateType_RECYCLED = ResourceStateType' "RECYCLED"

pattern ResourceStateType_RECYCLING :: ResourceStateType
pattern ResourceStateType_RECYCLING = ResourceStateType' "RECYCLING"

pattern ResourceStateType_RESTORING :: ResourceStateType
pattern ResourceStateType_RESTORING = ResourceStateType' "RESTORING"

{-# COMPLETE
  ResourceStateType_ACTIVE,
  ResourceStateType_RECYCLED,
  ResourceStateType_RECYCLING,
  ResourceStateType_RESTORING,
  ResourceStateType'
  #-}
