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
-- Module      : Amazonka.QuickSight.Types.VisualCustomActionTrigger
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.VisualCustomActionTrigger
  ( VisualCustomActionTrigger
      ( ..,
        VisualCustomActionTrigger_DATA_POINT_CLICK,
        VisualCustomActionTrigger_DATA_POINT_MENU
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype VisualCustomActionTrigger = VisualCustomActionTrigger'
  { fromVisualCustomActionTrigger ::
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

pattern VisualCustomActionTrigger_DATA_POINT_CLICK :: VisualCustomActionTrigger
pattern VisualCustomActionTrigger_DATA_POINT_CLICK = VisualCustomActionTrigger' "DATA_POINT_CLICK"

pattern VisualCustomActionTrigger_DATA_POINT_MENU :: VisualCustomActionTrigger
pattern VisualCustomActionTrigger_DATA_POINT_MENU = VisualCustomActionTrigger' "DATA_POINT_MENU"

{-# COMPLETE
  VisualCustomActionTrigger_DATA_POINT_CLICK,
  VisualCustomActionTrigger_DATA_POINT_MENU,
  VisualCustomActionTrigger'
  #-}
