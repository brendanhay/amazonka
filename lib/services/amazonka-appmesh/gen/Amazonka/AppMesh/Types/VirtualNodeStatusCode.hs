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
-- Module      : Amazonka.AppMesh.Types.VirtualNodeStatusCode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.VirtualNodeStatusCode
  ( VirtualNodeStatusCode
      ( ..,
        VirtualNodeStatusCode_ACTIVE,
        VirtualNodeStatusCode_DELETED,
        VirtualNodeStatusCode_INACTIVE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype VirtualNodeStatusCode = VirtualNodeStatusCode'
  { fromVirtualNodeStatusCode ::
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

pattern VirtualNodeStatusCode_ACTIVE :: VirtualNodeStatusCode
pattern VirtualNodeStatusCode_ACTIVE = VirtualNodeStatusCode' "ACTIVE"

pattern VirtualNodeStatusCode_DELETED :: VirtualNodeStatusCode
pattern VirtualNodeStatusCode_DELETED = VirtualNodeStatusCode' "DELETED"

pattern VirtualNodeStatusCode_INACTIVE :: VirtualNodeStatusCode
pattern VirtualNodeStatusCode_INACTIVE = VirtualNodeStatusCode' "INACTIVE"

{-# COMPLETE
  VirtualNodeStatusCode_ACTIVE,
  VirtualNodeStatusCode_DELETED,
  VirtualNodeStatusCode_INACTIVE,
  VirtualNodeStatusCode'
  #-}
