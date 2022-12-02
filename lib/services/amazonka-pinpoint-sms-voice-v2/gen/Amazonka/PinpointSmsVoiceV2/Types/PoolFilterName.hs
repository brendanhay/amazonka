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
-- Module      : Amazonka.PinpointSmsVoiceV2.Types.PoolFilterName
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PinpointSmsVoiceV2.Types.PoolFilterName
  ( PoolFilterName
      ( ..,
        PoolFilterName_Deletion_protection_enabled,
        PoolFilterName_Message_type,
        PoolFilterName_Opt_out_list_name,
        PoolFilterName_Self_managed_opt_outs_enabled,
        PoolFilterName_Shared_routes_enabled,
        PoolFilterName_Status,
        PoolFilterName_Two_way_enabled
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PoolFilterName = PoolFilterName'
  { fromPoolFilterName ::
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

pattern PoolFilterName_Deletion_protection_enabled :: PoolFilterName
pattern PoolFilterName_Deletion_protection_enabled = PoolFilterName' "deletion-protection-enabled"

pattern PoolFilterName_Message_type :: PoolFilterName
pattern PoolFilterName_Message_type = PoolFilterName' "message-type"

pattern PoolFilterName_Opt_out_list_name :: PoolFilterName
pattern PoolFilterName_Opt_out_list_name = PoolFilterName' "opt-out-list-name"

pattern PoolFilterName_Self_managed_opt_outs_enabled :: PoolFilterName
pattern PoolFilterName_Self_managed_opt_outs_enabled = PoolFilterName' "self-managed-opt-outs-enabled"

pattern PoolFilterName_Shared_routes_enabled :: PoolFilterName
pattern PoolFilterName_Shared_routes_enabled = PoolFilterName' "shared-routes-enabled"

pattern PoolFilterName_Status :: PoolFilterName
pattern PoolFilterName_Status = PoolFilterName' "status"

pattern PoolFilterName_Two_way_enabled :: PoolFilterName
pattern PoolFilterName_Two_way_enabled = PoolFilterName' "two-way-enabled"

{-# COMPLETE
  PoolFilterName_Deletion_protection_enabled,
  PoolFilterName_Message_type,
  PoolFilterName_Opt_out_list_name,
  PoolFilterName_Self_managed_opt_outs_enabled,
  PoolFilterName_Shared_routes_enabled,
  PoolFilterName_Status,
  PoolFilterName_Two_way_enabled,
  PoolFilterName'
  #-}
