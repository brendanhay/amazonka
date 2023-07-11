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
-- Module      : Amazonka.NetworkManager.Types.ChangeType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.ChangeType
  ( ChangeType
      ( ..,
        ChangeType_ATTACHMENT_MAPPING,
        ChangeType_ATTACHMENT_POLICIES_CONFIGURATION,
        ChangeType_ATTACHMENT_ROUTE_PROPAGATION,
        ChangeType_ATTACHMENT_ROUTE_STATIC,
        ChangeType_CORE_NETWORK_CONFIGURATION,
        ChangeType_CORE_NETWORK_EDGE,
        ChangeType_CORE_NETWORK_SEGMENT,
        ChangeType_SEGMENTS_CONFIGURATION,
        ChangeType_SEGMENT_ACTIONS_CONFIGURATION
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ChangeType = ChangeType'
  { fromChangeType ::
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

pattern ChangeType_ATTACHMENT_MAPPING :: ChangeType
pattern ChangeType_ATTACHMENT_MAPPING = ChangeType' "ATTACHMENT_MAPPING"

pattern ChangeType_ATTACHMENT_POLICIES_CONFIGURATION :: ChangeType
pattern ChangeType_ATTACHMENT_POLICIES_CONFIGURATION = ChangeType' "ATTACHMENT_POLICIES_CONFIGURATION"

pattern ChangeType_ATTACHMENT_ROUTE_PROPAGATION :: ChangeType
pattern ChangeType_ATTACHMENT_ROUTE_PROPAGATION = ChangeType' "ATTACHMENT_ROUTE_PROPAGATION"

pattern ChangeType_ATTACHMENT_ROUTE_STATIC :: ChangeType
pattern ChangeType_ATTACHMENT_ROUTE_STATIC = ChangeType' "ATTACHMENT_ROUTE_STATIC"

pattern ChangeType_CORE_NETWORK_CONFIGURATION :: ChangeType
pattern ChangeType_CORE_NETWORK_CONFIGURATION = ChangeType' "CORE_NETWORK_CONFIGURATION"

pattern ChangeType_CORE_NETWORK_EDGE :: ChangeType
pattern ChangeType_CORE_NETWORK_EDGE = ChangeType' "CORE_NETWORK_EDGE"

pattern ChangeType_CORE_NETWORK_SEGMENT :: ChangeType
pattern ChangeType_CORE_NETWORK_SEGMENT = ChangeType' "CORE_NETWORK_SEGMENT"

pattern ChangeType_SEGMENTS_CONFIGURATION :: ChangeType
pattern ChangeType_SEGMENTS_CONFIGURATION = ChangeType' "SEGMENTS_CONFIGURATION"

pattern ChangeType_SEGMENT_ACTIONS_CONFIGURATION :: ChangeType
pattern ChangeType_SEGMENT_ACTIONS_CONFIGURATION = ChangeType' "SEGMENT_ACTIONS_CONFIGURATION"

{-# COMPLETE
  ChangeType_ATTACHMENT_MAPPING,
  ChangeType_ATTACHMENT_POLICIES_CONFIGURATION,
  ChangeType_ATTACHMENT_ROUTE_PROPAGATION,
  ChangeType_ATTACHMENT_ROUTE_STATIC,
  ChangeType_CORE_NETWORK_CONFIGURATION,
  ChangeType_CORE_NETWORK_EDGE,
  ChangeType_CORE_NETWORK_SEGMENT,
  ChangeType_SEGMENTS_CONFIGURATION,
  ChangeType_SEGMENT_ACTIONS_CONFIGURATION,
  ChangeType'
  #-}
