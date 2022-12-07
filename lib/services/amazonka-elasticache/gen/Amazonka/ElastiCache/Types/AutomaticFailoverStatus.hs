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
-- Module      : Amazonka.ElastiCache.Types.AutomaticFailoverStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.AutomaticFailoverStatus
  ( AutomaticFailoverStatus
      ( ..,
        AutomaticFailoverStatus_Disabled,
        AutomaticFailoverStatus_Disabling,
        AutomaticFailoverStatus_Enabled,
        AutomaticFailoverStatus_Enabling
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AutomaticFailoverStatus = AutomaticFailoverStatus'
  { fromAutomaticFailoverStatus ::
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

pattern AutomaticFailoverStatus_Disabled :: AutomaticFailoverStatus
pattern AutomaticFailoverStatus_Disabled = AutomaticFailoverStatus' "disabled"

pattern AutomaticFailoverStatus_Disabling :: AutomaticFailoverStatus
pattern AutomaticFailoverStatus_Disabling = AutomaticFailoverStatus' "disabling"

pattern AutomaticFailoverStatus_Enabled :: AutomaticFailoverStatus
pattern AutomaticFailoverStatus_Enabled = AutomaticFailoverStatus' "enabled"

pattern AutomaticFailoverStatus_Enabling :: AutomaticFailoverStatus
pattern AutomaticFailoverStatus_Enabling = AutomaticFailoverStatus' "enabling"

{-# COMPLETE
  AutomaticFailoverStatus_Disabled,
  AutomaticFailoverStatus_Disabling,
  AutomaticFailoverStatus_Enabled,
  AutomaticFailoverStatus_Enabling,
  AutomaticFailoverStatus'
  #-}
