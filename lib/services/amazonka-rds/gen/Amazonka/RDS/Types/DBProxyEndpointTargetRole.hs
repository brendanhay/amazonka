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
-- Module      : Amazonka.RDS.Types.DBProxyEndpointTargetRole
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.DBProxyEndpointTargetRole
  ( DBProxyEndpointTargetRole
      ( ..,
        DBProxyEndpointTargetRole_READ_ONLY,
        DBProxyEndpointTargetRole_READ_WRITE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype DBProxyEndpointTargetRole = DBProxyEndpointTargetRole'
  { fromDBProxyEndpointTargetRole ::
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

pattern DBProxyEndpointTargetRole_READ_ONLY :: DBProxyEndpointTargetRole
pattern DBProxyEndpointTargetRole_READ_ONLY = DBProxyEndpointTargetRole' "READ_ONLY"

pattern DBProxyEndpointTargetRole_READ_WRITE :: DBProxyEndpointTargetRole
pattern DBProxyEndpointTargetRole_READ_WRITE = DBProxyEndpointTargetRole' "READ_WRITE"

{-# COMPLETE
  DBProxyEndpointTargetRole_READ_ONLY,
  DBProxyEndpointTargetRole_READ_WRITE,
  DBProxyEndpointTargetRole'
  #-}
