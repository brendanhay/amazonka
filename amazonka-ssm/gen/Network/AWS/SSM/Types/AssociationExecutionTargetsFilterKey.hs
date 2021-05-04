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
-- Module      : Network.AWS.SSM.Types.AssociationExecutionTargetsFilterKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AssociationExecutionTargetsFilterKey
  ( AssociationExecutionTargetsFilterKey
      ( ..,
        AssociationExecutionTargetsFilterKey_ResourceId,
        AssociationExecutionTargetsFilterKey_ResourceType,
        AssociationExecutionTargetsFilterKey_Status
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype AssociationExecutionTargetsFilterKey = AssociationExecutionTargetsFilterKey'
  { fromAssociationExecutionTargetsFilterKey ::
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

pattern AssociationExecutionTargetsFilterKey_ResourceId :: AssociationExecutionTargetsFilterKey
pattern AssociationExecutionTargetsFilterKey_ResourceId = AssociationExecutionTargetsFilterKey' "ResourceId"

pattern AssociationExecutionTargetsFilterKey_ResourceType :: AssociationExecutionTargetsFilterKey
pattern AssociationExecutionTargetsFilterKey_ResourceType = AssociationExecutionTargetsFilterKey' "ResourceType"

pattern AssociationExecutionTargetsFilterKey_Status :: AssociationExecutionTargetsFilterKey
pattern AssociationExecutionTargetsFilterKey_Status = AssociationExecutionTargetsFilterKey' "Status"

{-# COMPLETE
  AssociationExecutionTargetsFilterKey_ResourceId,
  AssociationExecutionTargetsFilterKey_ResourceType,
  AssociationExecutionTargetsFilterKey_Status,
  AssociationExecutionTargetsFilterKey'
  #-}
