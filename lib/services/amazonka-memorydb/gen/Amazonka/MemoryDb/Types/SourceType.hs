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
-- Module      : Amazonka.MemoryDb.Types.SourceType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MemoryDb.Types.SourceType
  ( SourceType
      ( ..,
        SourceType_Acl,
        SourceType_Cluster,
        SourceType_Node,
        SourceType_Parameter_group,
        SourceType_Subnet_group,
        SourceType_User
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SourceType = SourceType'
  { fromSourceType ::
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

pattern SourceType_Acl :: SourceType
pattern SourceType_Acl = SourceType' "acl"

pattern SourceType_Cluster :: SourceType
pattern SourceType_Cluster = SourceType' "cluster"

pattern SourceType_Node :: SourceType
pattern SourceType_Node = SourceType' "node"

pattern SourceType_Parameter_group :: SourceType
pattern SourceType_Parameter_group = SourceType' "parameter-group"

pattern SourceType_Subnet_group :: SourceType
pattern SourceType_Subnet_group = SourceType' "subnet-group"

pattern SourceType_User :: SourceType
pattern SourceType_User = SourceType' "user"

{-# COMPLETE
  SourceType_Acl,
  SourceType_Cluster,
  SourceType_Node,
  SourceType_Parameter_group,
  SourceType_Subnet_group,
  SourceType_User,
  SourceType'
  #-}
