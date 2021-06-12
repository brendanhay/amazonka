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
-- Module      : Network.AWS.SSM.Types.AssociationFilterKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AssociationFilterKey
  ( AssociationFilterKey
      ( ..,
        AssociationFilterKey_AssociationId,
        AssociationFilterKey_AssociationName,
        AssociationFilterKey_AssociationStatusName,
        AssociationFilterKey_InstanceId,
        AssociationFilterKey_LastExecutedAfter,
        AssociationFilterKey_LastExecutedBefore,
        AssociationFilterKey_Name,
        AssociationFilterKey_ResourceGroupName
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype AssociationFilterKey = AssociationFilterKey'
  { fromAssociationFilterKey ::
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

pattern AssociationFilterKey_AssociationId :: AssociationFilterKey
pattern AssociationFilterKey_AssociationId = AssociationFilterKey' "AssociationId"

pattern AssociationFilterKey_AssociationName :: AssociationFilterKey
pattern AssociationFilterKey_AssociationName = AssociationFilterKey' "AssociationName"

pattern AssociationFilterKey_AssociationStatusName :: AssociationFilterKey
pattern AssociationFilterKey_AssociationStatusName = AssociationFilterKey' "AssociationStatusName"

pattern AssociationFilterKey_InstanceId :: AssociationFilterKey
pattern AssociationFilterKey_InstanceId = AssociationFilterKey' "InstanceId"

pattern AssociationFilterKey_LastExecutedAfter :: AssociationFilterKey
pattern AssociationFilterKey_LastExecutedAfter = AssociationFilterKey' "LastExecutedAfter"

pattern AssociationFilterKey_LastExecutedBefore :: AssociationFilterKey
pattern AssociationFilterKey_LastExecutedBefore = AssociationFilterKey' "LastExecutedBefore"

pattern AssociationFilterKey_Name :: AssociationFilterKey
pattern AssociationFilterKey_Name = AssociationFilterKey' "Name"

pattern AssociationFilterKey_ResourceGroupName :: AssociationFilterKey
pattern AssociationFilterKey_ResourceGroupName = AssociationFilterKey' "ResourceGroupName"

{-# COMPLETE
  AssociationFilterKey_AssociationId,
  AssociationFilterKey_AssociationName,
  AssociationFilterKey_AssociationStatusName,
  AssociationFilterKey_InstanceId,
  AssociationFilterKey_LastExecutedAfter,
  AssociationFilterKey_LastExecutedBefore,
  AssociationFilterKey_Name,
  AssociationFilterKey_ResourceGroupName,
  AssociationFilterKey'
  #-}
