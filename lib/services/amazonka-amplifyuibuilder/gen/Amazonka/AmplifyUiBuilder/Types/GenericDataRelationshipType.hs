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
-- Module      : Amazonka.AmplifyUiBuilder.Types.GenericDataRelationshipType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.GenericDataRelationshipType
  ( GenericDataRelationshipType
      ( ..,
        GenericDataRelationshipType_BELONGS_TO,
        GenericDataRelationshipType_HAS_MANY,
        GenericDataRelationshipType_HAS_ONE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype GenericDataRelationshipType = GenericDataRelationshipType'
  { fromGenericDataRelationshipType ::
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

pattern GenericDataRelationshipType_BELONGS_TO :: GenericDataRelationshipType
pattern GenericDataRelationshipType_BELONGS_TO = GenericDataRelationshipType' "BELONGS_TO"

pattern GenericDataRelationshipType_HAS_MANY :: GenericDataRelationshipType
pattern GenericDataRelationshipType_HAS_MANY = GenericDataRelationshipType' "HAS_MANY"

pattern GenericDataRelationshipType_HAS_ONE :: GenericDataRelationshipType
pattern GenericDataRelationshipType_HAS_ONE = GenericDataRelationshipType' "HAS_ONE"

{-# COMPLETE
  GenericDataRelationshipType_BELONGS_TO,
  GenericDataRelationshipType_HAS_MANY,
  GenericDataRelationshipType_HAS_ONE,
  GenericDataRelationshipType'
  #-}
