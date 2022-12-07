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
-- Module      : Amazonka.AppMesh.Types.MeshStatusCode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.MeshStatusCode
  ( MeshStatusCode
      ( ..,
        MeshStatusCode_ACTIVE,
        MeshStatusCode_DELETED,
        MeshStatusCode_INACTIVE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype MeshStatusCode = MeshStatusCode'
  { fromMeshStatusCode ::
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

pattern MeshStatusCode_ACTIVE :: MeshStatusCode
pattern MeshStatusCode_ACTIVE = MeshStatusCode' "ACTIVE"

pattern MeshStatusCode_DELETED :: MeshStatusCode
pattern MeshStatusCode_DELETED = MeshStatusCode' "DELETED"

pattern MeshStatusCode_INACTIVE :: MeshStatusCode
pattern MeshStatusCode_INACTIVE = MeshStatusCode' "INACTIVE"

{-# COMPLETE
  MeshStatusCode_ACTIVE,
  MeshStatusCode_DELETED,
  MeshStatusCode_INACTIVE,
  MeshStatusCode'
  #-}
