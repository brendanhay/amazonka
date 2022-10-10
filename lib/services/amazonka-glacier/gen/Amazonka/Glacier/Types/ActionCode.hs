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
-- Module      : Amazonka.Glacier.Types.ActionCode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glacier.Types.ActionCode
  ( ActionCode
      ( ..,
        ActionCode_ArchiveRetrieval,
        ActionCode_InventoryRetrieval,
        ActionCode_Select
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ActionCode = ActionCode'
  { fromActionCode ::
      Core.Text
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
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern ActionCode_ArchiveRetrieval :: ActionCode
pattern ActionCode_ArchiveRetrieval = ActionCode' "ArchiveRetrieval"

pattern ActionCode_InventoryRetrieval :: ActionCode
pattern ActionCode_InventoryRetrieval = ActionCode' "InventoryRetrieval"

pattern ActionCode_Select :: ActionCode
pattern ActionCode_Select = ActionCode' "Select"

{-# COMPLETE
  ActionCode_ArchiveRetrieval,
  ActionCode_InventoryRetrieval,
  ActionCode_Select,
  ActionCode'
  #-}
