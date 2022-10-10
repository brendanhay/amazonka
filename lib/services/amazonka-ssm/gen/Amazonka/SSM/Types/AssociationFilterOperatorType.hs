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
-- Module      : Amazonka.SSM.Types.AssociationFilterOperatorType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.AssociationFilterOperatorType
  ( AssociationFilterOperatorType
      ( ..,
        AssociationFilterOperatorType_EQUAL,
        AssociationFilterOperatorType_GREATER_THAN,
        AssociationFilterOperatorType_LESS_THAN
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype AssociationFilterOperatorType = AssociationFilterOperatorType'
  { fromAssociationFilterOperatorType ::
      Core.Text
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

pattern AssociationFilterOperatorType_EQUAL :: AssociationFilterOperatorType
pattern AssociationFilterOperatorType_EQUAL = AssociationFilterOperatorType' "EQUAL"

pattern AssociationFilterOperatorType_GREATER_THAN :: AssociationFilterOperatorType
pattern AssociationFilterOperatorType_GREATER_THAN = AssociationFilterOperatorType' "GREATER_THAN"

pattern AssociationFilterOperatorType_LESS_THAN :: AssociationFilterOperatorType
pattern AssociationFilterOperatorType_LESS_THAN = AssociationFilterOperatorType' "LESS_THAN"

{-# COMPLETE
  AssociationFilterOperatorType_EQUAL,
  AssociationFilterOperatorType_GREATER_THAN,
  AssociationFilterOperatorType_LESS_THAN,
  AssociationFilterOperatorType'
  #-}
