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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
import qualified Amazonka.Prelude as Prelude

newtype ChoiceStatus = ChoiceStatus'
  { fromChoiceStatus ::
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
