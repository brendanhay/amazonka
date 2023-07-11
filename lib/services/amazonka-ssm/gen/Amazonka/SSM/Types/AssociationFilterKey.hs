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
-- Module      : Amazonka.SSM.Types.AssociationFilterKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.AssociationFilterKey
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AssociationFilterKey = AssociationFilterKey'
  { fromAssociationFilterKey ::
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
