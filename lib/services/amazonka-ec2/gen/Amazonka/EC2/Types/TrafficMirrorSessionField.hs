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
-- Module      : Amazonka.EC2.Types.TrafficMirrorSessionField
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TrafficMirrorSessionField
  ( TrafficMirrorSessionField
      ( ..,
        TrafficMirrorSessionField_Description,
        TrafficMirrorSessionField_Packet_length,
        TrafficMirrorSessionField_Virtual_network_id
      ),
  )
where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype TrafficMirrorSessionField = TrafficMirrorSessionField'
  { fromTrafficMirrorSessionField ::
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

pattern TrafficMirrorSessionField_Description :: TrafficMirrorSessionField
pattern TrafficMirrorSessionField_Description = TrafficMirrorSessionField' "description"

pattern TrafficMirrorSessionField_Packet_length :: TrafficMirrorSessionField
pattern TrafficMirrorSessionField_Packet_length = TrafficMirrorSessionField' "packet-length"

pattern TrafficMirrorSessionField_Virtual_network_id :: TrafficMirrorSessionField
pattern TrafficMirrorSessionField_Virtual_network_id = TrafficMirrorSessionField' "virtual-network-id"

{-# COMPLETE
  TrafficMirrorSessionField_Description,
  TrafficMirrorSessionField_Packet_length,
  TrafficMirrorSessionField_Virtual_network_id,
  TrafficMirrorSessionField'
  #-}
