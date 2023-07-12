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
-- Module      : Amazonka.LexV2Models.Types.SlotTypeCategory
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.SlotTypeCategory
  ( SlotTypeCategory
      ( ..,
        SlotTypeCategory_Composite,
        SlotTypeCategory_Custom,
        SlotTypeCategory_Extended,
        SlotTypeCategory_ExternalGrammar
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SlotTypeCategory = SlotTypeCategory'
  { fromSlotTypeCategory ::
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

pattern SlotTypeCategory_Composite :: SlotTypeCategory
pattern SlotTypeCategory_Composite = SlotTypeCategory' "Composite"

pattern SlotTypeCategory_Custom :: SlotTypeCategory
pattern SlotTypeCategory_Custom = SlotTypeCategory' "Custom"

pattern SlotTypeCategory_Extended :: SlotTypeCategory
pattern SlotTypeCategory_Extended = SlotTypeCategory' "Extended"

pattern SlotTypeCategory_ExternalGrammar :: SlotTypeCategory
pattern SlotTypeCategory_ExternalGrammar = SlotTypeCategory' "ExternalGrammar"

{-# COMPLETE
  SlotTypeCategory_Composite,
  SlotTypeCategory_Custom,
  SlotTypeCategory_Extended,
  SlotTypeCategory_ExternalGrammar,
  SlotTypeCategory'
  #-}
