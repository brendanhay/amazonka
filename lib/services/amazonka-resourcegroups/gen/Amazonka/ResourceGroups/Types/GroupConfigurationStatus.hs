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
-- Module      : Amazonka.ResourceGroups.Types.GroupConfigurationStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResourceGroups.Types.GroupConfigurationStatus
  ( GroupConfigurationStatus
      ( ..,
        GroupConfigurationStatus_UPDATE_COMPLETE,
        GroupConfigurationStatus_UPDATE_FAILED,
        GroupConfigurationStatus_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype GroupConfigurationStatus = GroupConfigurationStatus'
  { fromGroupConfigurationStatus ::
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

pattern GroupConfigurationStatus_UPDATE_COMPLETE :: GroupConfigurationStatus
pattern GroupConfigurationStatus_UPDATE_COMPLETE = GroupConfigurationStatus' "UPDATE_COMPLETE"

pattern GroupConfigurationStatus_UPDATE_FAILED :: GroupConfigurationStatus
pattern GroupConfigurationStatus_UPDATE_FAILED = GroupConfigurationStatus' "UPDATE_FAILED"

pattern GroupConfigurationStatus_UPDATING :: GroupConfigurationStatus
pattern GroupConfigurationStatus_UPDATING = GroupConfigurationStatus' "UPDATING"

{-# COMPLETE
  GroupConfigurationStatus_UPDATE_COMPLETE,
  GroupConfigurationStatus_UPDATE_FAILED,
  GroupConfigurationStatus_UPDATING,
  GroupConfigurationStatus'
  #-}
