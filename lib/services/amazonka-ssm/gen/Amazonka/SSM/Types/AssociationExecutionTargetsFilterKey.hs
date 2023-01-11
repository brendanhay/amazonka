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
-- Module      : Amazonka.SSM.Types.AssociationExecutionTargetsFilterKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.AssociationExecutionTargetsFilterKey
  ( AssociationExecutionTargetsFilterKey
      ( ..,
        AssociationExecutionTargetsFilterKey_ResourceId,
        AssociationExecutionTargetsFilterKey_ResourceType,
        AssociationExecutionTargetsFilterKey_Status
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AssociationExecutionTargetsFilterKey = AssociationExecutionTargetsFilterKey'
  { fromAssociationExecutionTargetsFilterKey ::
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
