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
-- Module      : Amazonka.WorkDocs.Types.OrderByFieldType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkDocs.Types.OrderByFieldType
  ( OrderByFieldType
      ( ..,
        OrderByFieldType_CREATED_TIMESTAMP,
        OrderByFieldType_MODIFIED_TIMESTAMP,
        OrderByFieldType_NAME,
        OrderByFieldType_RELEVANCE,
        OrderByFieldType_SIZE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype OrderByFieldType = OrderByFieldType'
  { fromOrderByFieldType ::
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

pattern OrderByFieldType_CREATED_TIMESTAMP :: OrderByFieldType
pattern OrderByFieldType_CREATED_TIMESTAMP = OrderByFieldType' "CREATED_TIMESTAMP"

pattern OrderByFieldType_MODIFIED_TIMESTAMP :: OrderByFieldType
pattern OrderByFieldType_MODIFIED_TIMESTAMP = OrderByFieldType' "MODIFIED_TIMESTAMP"

pattern OrderByFieldType_NAME :: OrderByFieldType
pattern OrderByFieldType_NAME = OrderByFieldType' "NAME"

pattern OrderByFieldType_RELEVANCE :: OrderByFieldType
pattern OrderByFieldType_RELEVANCE = OrderByFieldType' "RELEVANCE"

pattern OrderByFieldType_SIZE :: OrderByFieldType
pattern OrderByFieldType_SIZE = OrderByFieldType' "SIZE"

{-# COMPLETE
  OrderByFieldType_CREATED_TIMESTAMP,
  OrderByFieldType_MODIFIED_TIMESTAMP,
  OrderByFieldType_NAME,
  OrderByFieldType_RELEVANCE,
  OrderByFieldType_SIZE,
  OrderByFieldType'
  #-}
