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

import qualified Network.AWS.Prelude as Prelude

newtype AssociationFilterKey = AssociationFilterKey'
  { fromAssociationFilterKey ::
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
