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
-- Module      : Network.AWS.Glacier.Types.ActionCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.ActionCode
  ( ActionCode
      ( ..,
        ActionCode_ArchiveRetrieval,
        ActionCode_InventoryRetrieval,
        ActionCode_Select
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ActionCode = ActionCode'
  { fromActionCode ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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
