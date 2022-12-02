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
-- Module      : Amazonka.CodeStarNotifications.Types.ListEventTypesFilterName
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeStarNotifications.Types.ListEventTypesFilterName
  ( ListEventTypesFilterName
      ( ..,
        ListEventTypesFilterName_RESOURCE_TYPE,
        ListEventTypesFilterName_SERVICE_NAME
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ListEventTypesFilterName = ListEventTypesFilterName'
  { fromListEventTypesFilterName ::
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

pattern ListEventTypesFilterName_RESOURCE_TYPE :: ListEventTypesFilterName
pattern ListEventTypesFilterName_RESOURCE_TYPE = ListEventTypesFilterName' "RESOURCE_TYPE"

pattern ListEventTypesFilterName_SERVICE_NAME :: ListEventTypesFilterName
pattern ListEventTypesFilterName_SERVICE_NAME = ListEventTypesFilterName' "SERVICE_NAME"

{-# COMPLETE
  ListEventTypesFilterName_RESOURCE_TYPE,
  ListEventTypesFilterName_SERVICE_NAME,
  ListEventTypesFilterName'
  #-}
