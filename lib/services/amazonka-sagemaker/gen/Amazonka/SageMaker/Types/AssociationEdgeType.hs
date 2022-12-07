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
-- Module      : Amazonka.SageMaker.Types.AssociationEdgeType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.AssociationEdgeType
  ( AssociationEdgeType
      ( ..,
        AssociationEdgeType_AssociatedWith,
        AssociationEdgeType_ContributedTo,
        AssociationEdgeType_DerivedFrom,
        AssociationEdgeType_Produced
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AssociationEdgeType = AssociationEdgeType'
  { fromAssociationEdgeType ::
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

pattern AssociationEdgeType_AssociatedWith :: AssociationEdgeType
pattern AssociationEdgeType_AssociatedWith = AssociationEdgeType' "AssociatedWith"

pattern AssociationEdgeType_ContributedTo :: AssociationEdgeType
pattern AssociationEdgeType_ContributedTo = AssociationEdgeType' "ContributedTo"

pattern AssociationEdgeType_DerivedFrom :: AssociationEdgeType
pattern AssociationEdgeType_DerivedFrom = AssociationEdgeType' "DerivedFrom"

pattern AssociationEdgeType_Produced :: AssociationEdgeType
pattern AssociationEdgeType_Produced = AssociationEdgeType' "Produced"

{-# COMPLETE
  AssociationEdgeType_AssociatedWith,
  AssociationEdgeType_ContributedTo,
  AssociationEdgeType_DerivedFrom,
  AssociationEdgeType_Produced,
  AssociationEdgeType'
  #-}
