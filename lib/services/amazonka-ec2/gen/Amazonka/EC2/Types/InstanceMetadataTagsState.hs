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
-- Module      : Amazonka.EC2.Types.InstanceMetadataTagsState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.InstanceMetadataTagsState
  ( InstanceMetadataTagsState
      ( ..,
        InstanceMetadataTagsState_Disabled,
        InstanceMetadataTagsState_Enabled
      ),
  )
where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype InstanceMetadataTagsState = InstanceMetadataTagsState'
  { fromInstanceMetadataTagsState ::
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

pattern InstanceMetadataTagsState_Disabled :: InstanceMetadataTagsState
pattern InstanceMetadataTagsState_Disabled = InstanceMetadataTagsState' "disabled"

pattern InstanceMetadataTagsState_Enabled :: InstanceMetadataTagsState
pattern InstanceMetadataTagsState_Enabled = InstanceMetadataTagsState' "enabled"

{-# COMPLETE
  InstanceMetadataTagsState_Disabled,
  InstanceMetadataTagsState_Enabled,
  InstanceMetadataTagsState'
  #-}
