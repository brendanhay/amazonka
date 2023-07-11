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
-- Module      : Amazonka.WellArchitected.Types.ChoiceStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.ChoiceStatus
  ( ChoiceStatus
      ( ..,
        ChoiceStatus_NOT_APPLICABLE,
        ChoiceStatus_SELECTED,
        ChoiceStatus_UNSELECTED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ChoiceStatus = ChoiceStatus'
  { fromChoiceStatus ::
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

pattern ChoiceStatus_NOT_APPLICABLE :: ChoiceStatus
pattern ChoiceStatus_NOT_APPLICABLE = ChoiceStatus' "NOT_APPLICABLE"

pattern ChoiceStatus_SELECTED :: ChoiceStatus
pattern ChoiceStatus_SELECTED = ChoiceStatus' "SELECTED"

pattern ChoiceStatus_UNSELECTED :: ChoiceStatus
pattern ChoiceStatus_UNSELECTED = ChoiceStatus' "UNSELECTED"

{-# COMPLETE
  ChoiceStatus_NOT_APPLICABLE,
  ChoiceStatus_SELECTED,
  ChoiceStatus_UNSELECTED,
  ChoiceStatus'
  #-}
