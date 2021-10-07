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
-- Module      : Network.AWS.ECS.Types.NetworkMode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.NetworkMode
  ( NetworkMode
      ( ..,
        NetworkMode_Awsvpc,
        NetworkMode_Bridge,
        NetworkMode_Host,
        NetworkMode_None
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype NetworkMode = NetworkMode'
  { fromNetworkMode ::
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

pattern NetworkMode_Awsvpc :: NetworkMode
pattern NetworkMode_Awsvpc = NetworkMode' "awsvpc"

pattern NetworkMode_Bridge :: NetworkMode
pattern NetworkMode_Bridge = NetworkMode' "bridge"

pattern NetworkMode_Host :: NetworkMode
pattern NetworkMode_Host = NetworkMode' "host"

pattern NetworkMode_None :: NetworkMode
pattern NetworkMode_None = NetworkMode' "none"

{-# COMPLETE
  NetworkMode_Awsvpc,
  NetworkMode_Bridge,
  NetworkMode_Host,
  NetworkMode_None,
  NetworkMode'
  #-}
