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
-- Module      : Amazonka.Textract.Types.RelationshipType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Textract.Types.RelationshipType
  ( RelationshipType
      ( ..,
        RelationshipType_ANSWER,
        RelationshipType_CHILD,
        RelationshipType_COMPLEX_FEATURES,
        RelationshipType_MERGED_CELL,
        RelationshipType_TITLE,
        RelationshipType_VALUE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype RelationshipType = RelationshipType'
  { fromRelationshipType ::
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

pattern RelationshipType_ANSWER :: RelationshipType
pattern RelationshipType_ANSWER = RelationshipType' "ANSWER"

pattern RelationshipType_CHILD :: RelationshipType
pattern RelationshipType_CHILD = RelationshipType' "CHILD"

pattern RelationshipType_COMPLEX_FEATURES :: RelationshipType
pattern RelationshipType_COMPLEX_FEATURES = RelationshipType' "COMPLEX_FEATURES"

pattern RelationshipType_MERGED_CELL :: RelationshipType
pattern RelationshipType_MERGED_CELL = RelationshipType' "MERGED_CELL"

pattern RelationshipType_TITLE :: RelationshipType
pattern RelationshipType_TITLE = RelationshipType' "TITLE"

pattern RelationshipType_VALUE :: RelationshipType
pattern RelationshipType_VALUE = RelationshipType' "VALUE"

{-# COMPLETE
  RelationshipType_ANSWER,
  RelationshipType_CHILD,
  RelationshipType_COMPLEX_FEATURES,
  RelationshipType_MERGED_CELL,
  RelationshipType_TITLE,
  RelationshipType_VALUE,
  RelationshipType'
  #-}
