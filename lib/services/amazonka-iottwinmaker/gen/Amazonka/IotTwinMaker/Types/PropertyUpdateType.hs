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
-- Module      : Amazonka.IotTwinMaker.Types.PropertyUpdateType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Types.PropertyUpdateType
  ( PropertyUpdateType
      ( ..,
        PropertyUpdateType_CREATE,
        PropertyUpdateType_DELETE,
        PropertyUpdateType_UPDATE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PropertyUpdateType = PropertyUpdateType'
  { fromPropertyUpdateType ::
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

pattern PropertyUpdateType_CREATE :: PropertyUpdateType
pattern PropertyUpdateType_CREATE = PropertyUpdateType' "CREATE"

pattern PropertyUpdateType_DELETE :: PropertyUpdateType
pattern PropertyUpdateType_DELETE = PropertyUpdateType' "DELETE"

pattern PropertyUpdateType_UPDATE :: PropertyUpdateType
pattern PropertyUpdateType_UPDATE = PropertyUpdateType' "UPDATE"

{-# COMPLETE
  PropertyUpdateType_CREATE,
  PropertyUpdateType_DELETE,
  PropertyUpdateType_UPDATE,
  PropertyUpdateType'
  #-}
