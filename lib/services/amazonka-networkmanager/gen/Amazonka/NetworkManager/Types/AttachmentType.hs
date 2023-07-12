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
-- Module      : Amazonka.NetworkManager.Types.AttachmentType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.AttachmentType
  ( AttachmentType
      ( ..,
        AttachmentType_CONNECT,
        AttachmentType_SITE_TO_SITE_VPN,
        AttachmentType_TRANSIT_GATEWAY_ROUTE_TABLE,
        AttachmentType_VPC
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AttachmentType = AttachmentType'
  { fromAttachmentType ::
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

pattern AttachmentType_CONNECT :: AttachmentType
pattern AttachmentType_CONNECT = AttachmentType' "CONNECT"

pattern AttachmentType_SITE_TO_SITE_VPN :: AttachmentType
pattern AttachmentType_SITE_TO_SITE_VPN = AttachmentType' "SITE_TO_SITE_VPN"

pattern AttachmentType_TRANSIT_GATEWAY_ROUTE_TABLE :: AttachmentType
pattern AttachmentType_TRANSIT_GATEWAY_ROUTE_TABLE = AttachmentType' "TRANSIT_GATEWAY_ROUTE_TABLE"

pattern AttachmentType_VPC :: AttachmentType
pattern AttachmentType_VPC = AttachmentType' "VPC"

{-# COMPLETE
  AttachmentType_CONNECT,
  AttachmentType_SITE_TO_SITE_VPN,
  AttachmentType_TRANSIT_GATEWAY_ROUTE_TABLE,
  AttachmentType_VPC,
  AttachmentType'
  #-}
