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
-- Module      : Amazonka.MacieV2.Types.ClassificationScopeUpdateOperation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.ClassificationScopeUpdateOperation
  ( ClassificationScopeUpdateOperation
      ( ..,
        ClassificationScopeUpdateOperation_ADD,
        ClassificationScopeUpdateOperation_REMOVE,
        ClassificationScopeUpdateOperation_REPLACE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies how to apply changes to the S3 bucket exclusion list defined
-- by the classification scope for an Amazon Macie account. Valid values
-- are:
newtype ClassificationScopeUpdateOperation = ClassificationScopeUpdateOperation'
  { fromClassificationScopeUpdateOperation ::
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

pattern ClassificationScopeUpdateOperation_ADD :: ClassificationScopeUpdateOperation
pattern ClassificationScopeUpdateOperation_ADD = ClassificationScopeUpdateOperation' "ADD"

pattern ClassificationScopeUpdateOperation_REMOVE :: ClassificationScopeUpdateOperation
pattern ClassificationScopeUpdateOperation_REMOVE = ClassificationScopeUpdateOperation' "REMOVE"

pattern ClassificationScopeUpdateOperation_REPLACE :: ClassificationScopeUpdateOperation
pattern ClassificationScopeUpdateOperation_REPLACE = ClassificationScopeUpdateOperation' "REPLACE"

{-# COMPLETE
  ClassificationScopeUpdateOperation_ADD,
  ClassificationScopeUpdateOperation_REMOVE,
  ClassificationScopeUpdateOperation_REPLACE,
  ClassificationScopeUpdateOperation'
  #-}
