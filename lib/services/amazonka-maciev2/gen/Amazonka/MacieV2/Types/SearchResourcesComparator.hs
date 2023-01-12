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
-- Module      : Amazonka.MacieV2.Types.SearchResourcesComparator
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.SearchResourcesComparator
  ( SearchResourcesComparator
      ( ..,
        SearchResourcesComparator_EQ,
        SearchResourcesComparator_NE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The operator to use in a condition that filters the results of a query.
-- Valid values are:
newtype SearchResourcesComparator = SearchResourcesComparator'
  { fromSearchResourcesComparator ::
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

pattern SearchResourcesComparator_EQ :: SearchResourcesComparator
pattern SearchResourcesComparator_EQ = SearchResourcesComparator' "EQ"

pattern SearchResourcesComparator_NE :: SearchResourcesComparator
pattern SearchResourcesComparator_NE = SearchResourcesComparator' "NE"

{-# COMPLETE
  SearchResourcesComparator_EQ,
  SearchResourcesComparator_NE,
  SearchResourcesComparator'
  #-}
