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
-- Module      : Amazonka.Redshift.Types.AquaConfigurationStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.AquaConfigurationStatus
  ( AquaConfigurationStatus
      ( ..,
        AquaConfigurationStatus_Auto,
        AquaConfigurationStatus_Disabled,
        AquaConfigurationStatus_Enabled
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal

newtype AquaConfigurationStatus = AquaConfigurationStatus'
  { fromAquaConfigurationStatus ::
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

pattern AquaConfigurationStatus_Auto :: AquaConfigurationStatus
pattern AquaConfigurationStatus_Auto = AquaConfigurationStatus' "auto"

pattern AquaConfigurationStatus_Disabled :: AquaConfigurationStatus
pattern AquaConfigurationStatus_Disabled = AquaConfigurationStatus' "disabled"

pattern AquaConfigurationStatus_Enabled :: AquaConfigurationStatus
pattern AquaConfigurationStatus_Enabled = AquaConfigurationStatus' "enabled"

{-# COMPLETE
  AquaConfigurationStatus_Auto,
  AquaConfigurationStatus_Disabled,
  AquaConfigurationStatus_Enabled,
  AquaConfigurationStatus'
  #-}
