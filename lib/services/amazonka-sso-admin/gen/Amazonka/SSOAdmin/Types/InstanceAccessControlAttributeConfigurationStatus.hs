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
-- Module      : Amazonka.SSOAdmin.Types.InstanceAccessControlAttributeConfigurationStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSOAdmin.Types.InstanceAccessControlAttributeConfigurationStatus
  ( InstanceAccessControlAttributeConfigurationStatus
      ( ..,
        InstanceAccessControlAttributeConfigurationStatus_CREATION_FAILED,
        InstanceAccessControlAttributeConfigurationStatus_CREATION_IN_PROGRESS,
        InstanceAccessControlAttributeConfigurationStatus_ENABLED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype InstanceAccessControlAttributeConfigurationStatus = InstanceAccessControlAttributeConfigurationStatus'
  { fromInstanceAccessControlAttributeConfigurationStatus ::
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

pattern InstanceAccessControlAttributeConfigurationStatus_CREATION_FAILED :: InstanceAccessControlAttributeConfigurationStatus
pattern InstanceAccessControlAttributeConfigurationStatus_CREATION_FAILED = InstanceAccessControlAttributeConfigurationStatus' "CREATION_FAILED"

pattern InstanceAccessControlAttributeConfigurationStatus_CREATION_IN_PROGRESS :: InstanceAccessControlAttributeConfigurationStatus
pattern InstanceAccessControlAttributeConfigurationStatus_CREATION_IN_PROGRESS = InstanceAccessControlAttributeConfigurationStatus' "CREATION_IN_PROGRESS"

pattern InstanceAccessControlAttributeConfigurationStatus_ENABLED :: InstanceAccessControlAttributeConfigurationStatus
pattern InstanceAccessControlAttributeConfigurationStatus_ENABLED = InstanceAccessControlAttributeConfigurationStatus' "ENABLED"

{-# COMPLETE
  InstanceAccessControlAttributeConfigurationStatus_CREATION_FAILED,
  InstanceAccessControlAttributeConfigurationStatus_CREATION_IN_PROGRESS,
  InstanceAccessControlAttributeConfigurationStatus_ENABLED,
  InstanceAccessControlAttributeConfigurationStatus'
  #-}
