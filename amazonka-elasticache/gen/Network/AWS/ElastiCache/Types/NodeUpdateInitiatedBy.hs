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
-- Module      : Network.AWS.ElastiCache.Types.NodeUpdateInitiatedBy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.NodeUpdateInitiatedBy
  ( NodeUpdateInitiatedBy
      ( ..,
        NodeUpdateInitiatedBy_Customer,
        NodeUpdateInitiatedBy_System
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype NodeUpdateInitiatedBy = NodeUpdateInitiatedBy'
  { fromNodeUpdateInitiatedBy ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern NodeUpdateInitiatedBy_Customer :: NodeUpdateInitiatedBy
pattern NodeUpdateInitiatedBy_Customer = NodeUpdateInitiatedBy' "customer"

pattern NodeUpdateInitiatedBy_System :: NodeUpdateInitiatedBy
pattern NodeUpdateInitiatedBy_System = NodeUpdateInitiatedBy' "system"

{-# COMPLETE
  NodeUpdateInitiatedBy_Customer,
  NodeUpdateInitiatedBy_System,
  NodeUpdateInitiatedBy'
  #-}
