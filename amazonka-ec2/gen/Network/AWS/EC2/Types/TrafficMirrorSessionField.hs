{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TrafficMirrorSessionField
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TrafficMirrorSessionField
  ( TrafficMirrorSessionField
      ( ..,
        TrafficMirrorSessionField_Description,
        TrafficMirrorSessionField_Packet_length,
        TrafficMirrorSessionField_Virtual_network_id
      ),
  )
where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Prelude as Prelude

newtype TrafficMirrorSessionField = TrafficMirrorSessionField'
  { fromTrafficMirrorSessionField ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
