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
-- Module      : Amazonka.CloudDirectory.Types.ObjectType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.ObjectType
  ( ObjectType
      ( ..,
        ObjectType_INDEX,
        ObjectType_LEAF_NODE,
        ObjectType_NODE,
        ObjectType_POLICY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ObjectType = ObjectType'
  { fromObjectType ::
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

pattern ObjectType_INDEX :: ObjectType
pattern ObjectType_INDEX = ObjectType' "INDEX"

pattern ObjectType_LEAF_NODE :: ObjectType
pattern ObjectType_LEAF_NODE = ObjectType' "LEAF_NODE"

pattern ObjectType_NODE :: ObjectType
pattern ObjectType_NODE = ObjectType' "NODE"

pattern ObjectType_POLICY :: ObjectType
pattern ObjectType_POLICY = ObjectType' "POLICY"

{-# COMPLETE
  ObjectType_INDEX,
  ObjectType_LEAF_NODE,
  ObjectType_NODE,
  ObjectType_POLICY,
  ObjectType'
  #-}
