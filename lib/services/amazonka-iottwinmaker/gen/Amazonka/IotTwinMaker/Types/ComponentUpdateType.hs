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
-- Module      : Amazonka.IotTwinMaker.Types.ComponentUpdateType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Types.ComponentUpdateType
  ( ComponentUpdateType
      ( ..,
        ComponentUpdateType_CREATE,
        ComponentUpdateType_DELETE,
        ComponentUpdateType_UPDATE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ComponentUpdateType = ComponentUpdateType'
  { fromComponentUpdateType ::
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

pattern ComponentUpdateType_CREATE :: ComponentUpdateType
pattern ComponentUpdateType_CREATE = ComponentUpdateType' "CREATE"

pattern ComponentUpdateType_DELETE :: ComponentUpdateType
pattern ComponentUpdateType_DELETE = ComponentUpdateType' "DELETE"

pattern ComponentUpdateType_UPDATE :: ComponentUpdateType
pattern ComponentUpdateType_UPDATE = ComponentUpdateType' "UPDATE"

{-# COMPLETE
  ComponentUpdateType_CREATE,
  ComponentUpdateType_DELETE,
  ComponentUpdateType_UPDATE,
  ComponentUpdateType'
  #-}
