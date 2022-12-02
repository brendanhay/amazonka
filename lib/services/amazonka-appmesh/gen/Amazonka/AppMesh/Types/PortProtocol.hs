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
-- Module      : Amazonka.AppMesh.Types.PortProtocol
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.PortProtocol
  ( PortProtocol
      ( ..,
        PortProtocol_Grpc,
        PortProtocol_Http,
        PortProtocol_Http2,
        PortProtocol_Tcp
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PortProtocol = PortProtocol'
  { fromPortProtocol ::
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

pattern PortProtocol_Grpc :: PortProtocol
pattern PortProtocol_Grpc = PortProtocol' "grpc"

pattern PortProtocol_Http :: PortProtocol
pattern PortProtocol_Http = PortProtocol' "http"

pattern PortProtocol_Http2 :: PortProtocol
pattern PortProtocol_Http2 = PortProtocol' "http2"

pattern PortProtocol_Tcp :: PortProtocol
pattern PortProtocol_Tcp = PortProtocol' "tcp"

{-# COMPLETE
  PortProtocol_Grpc,
  PortProtocol_Http,
  PortProtocol_Http2,
  PortProtocol_Tcp,
  PortProtocol'
  #-}
