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
-- Module      : Network.AWS.SageMaker.Types.AssociationEdgeType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AssociationEdgeType
  ( AssociationEdgeType
      ( ..,
        AssociationEdgeType_AssociatedWith,
        AssociationEdgeType_ContributedTo,
        AssociationEdgeType_DerivedFrom,
        AssociationEdgeType_Produced
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype AssociationEdgeType = AssociationEdgeType'
  { fromAssociationEdgeType ::
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
