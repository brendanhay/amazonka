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
-- Module      : Amazonka.VPCLattice.Types.TargetGroupProtocolVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VPCLattice.Types.TargetGroupProtocolVersion
  ( TargetGroupProtocolVersion
      ( ..,
        TargetGroupProtocolVersion_GRPC,
        TargetGroupProtocolVersion_HTTP1,
        TargetGroupProtocolVersion_HTTP2
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TargetGroupProtocolVersion = TargetGroupProtocolVersion'
  { fromTargetGroupProtocolVersion ::
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

pattern TargetGroupProtocolVersion_GRPC :: TargetGroupProtocolVersion
pattern TargetGroupProtocolVersion_GRPC = TargetGroupProtocolVersion' "GRPC"

pattern TargetGroupProtocolVersion_HTTP1 :: TargetGroupProtocolVersion
pattern TargetGroupProtocolVersion_HTTP1 = TargetGroupProtocolVersion' "HTTP1"

pattern TargetGroupProtocolVersion_HTTP2 :: TargetGroupProtocolVersion
pattern TargetGroupProtocolVersion_HTTP2 = TargetGroupProtocolVersion' "HTTP2"

{-# COMPLETE
  TargetGroupProtocolVersion_GRPC,
  TargetGroupProtocolVersion_HTTP1,
  TargetGroupProtocolVersion_HTTP2,
  TargetGroupProtocolVersion'
  #-}
