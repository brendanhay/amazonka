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
-- Module      : Amazonka.CodeStarNotifications.Types.ListTargetsFilterName
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeStarNotifications.Types.ListTargetsFilterName
  ( ListTargetsFilterName
      ( ..,
        ListTargetsFilterName_TARGET_ADDRESS,
        ListTargetsFilterName_TARGET_STATUS,
        ListTargetsFilterName_TARGET_TYPE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ListTargetsFilterName = ListTargetsFilterName'
  { fromListTargetsFilterName ::
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

pattern ListTargetsFilterName_TARGET_ADDRESS :: ListTargetsFilterName
pattern ListTargetsFilterName_TARGET_ADDRESS = ListTargetsFilterName' "TARGET_ADDRESS"

pattern ListTargetsFilterName_TARGET_STATUS :: ListTargetsFilterName
pattern ListTargetsFilterName_TARGET_STATUS = ListTargetsFilterName' "TARGET_STATUS"

pattern ListTargetsFilterName_TARGET_TYPE :: ListTargetsFilterName
pattern ListTargetsFilterName_TARGET_TYPE = ListTargetsFilterName' "TARGET_TYPE"

{-# COMPLETE
  ListTargetsFilterName_TARGET_ADDRESS,
  ListTargetsFilterName_TARGET_STATUS,
  ListTargetsFilterName_TARGET_TYPE,
  ListTargetsFilterName'
  #-}
