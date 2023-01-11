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
-- Module      : Amazonka.CloudWatch.Types.ActionsSuppressedBy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatch.Types.ActionsSuppressedBy
  ( ActionsSuppressedBy
      ( ..,
        ActionsSuppressedBy_Alarm,
        ActionsSuppressedBy_ExtensionPeriod,
        ActionsSuppressedBy_WaitPeriod
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ActionsSuppressedBy = ActionsSuppressedBy'
  { fromActionsSuppressedBy ::
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

pattern ActionsSuppressedBy_Alarm :: ActionsSuppressedBy
pattern ActionsSuppressedBy_Alarm = ActionsSuppressedBy' "Alarm"

pattern ActionsSuppressedBy_ExtensionPeriod :: ActionsSuppressedBy
pattern ActionsSuppressedBy_ExtensionPeriod = ActionsSuppressedBy' "ExtensionPeriod"

pattern ActionsSuppressedBy_WaitPeriod :: ActionsSuppressedBy
pattern ActionsSuppressedBy_WaitPeriod = ActionsSuppressedBy' "WaitPeriod"

{-# COMPLETE
  ActionsSuppressedBy_Alarm,
  ActionsSuppressedBy_ExtensionPeriod,
  ActionsSuppressedBy_WaitPeriod,
  ActionsSuppressedBy'
  #-}
