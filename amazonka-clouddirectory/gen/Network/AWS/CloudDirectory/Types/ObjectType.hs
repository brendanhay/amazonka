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
-- Module      : Network.AWS.CloudDirectory.Types.ObjectType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.ObjectType
  ( ObjectType
      ( ..,
        ObjectType_INDEX,
        ObjectType_LEAF_NODE,
        ObjectType_NODE,
        ObjectType_POLICY
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ObjectType = ObjectType'
  { fromObjectType ::
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
