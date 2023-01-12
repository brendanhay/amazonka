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
-- Module      : Amazonka.SageMaker.Types.ListDeviceFleetsSortBy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ListDeviceFleetsSortBy
  ( ListDeviceFleetsSortBy
      ( ..,
        ListDeviceFleetsSortBy_CREATION_TIME,
        ListDeviceFleetsSortBy_LAST_MODIFIED_TIME,
        ListDeviceFleetsSortBy_NAME
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ListDeviceFleetsSortBy = ListDeviceFleetsSortBy'
  { fromListDeviceFleetsSortBy ::
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

pattern ListDeviceFleetsSortBy_CREATION_TIME :: ListDeviceFleetsSortBy
pattern ListDeviceFleetsSortBy_CREATION_TIME = ListDeviceFleetsSortBy' "CREATION_TIME"

pattern ListDeviceFleetsSortBy_LAST_MODIFIED_TIME :: ListDeviceFleetsSortBy
pattern ListDeviceFleetsSortBy_LAST_MODIFIED_TIME = ListDeviceFleetsSortBy' "LAST_MODIFIED_TIME"

pattern ListDeviceFleetsSortBy_NAME :: ListDeviceFleetsSortBy
pattern ListDeviceFleetsSortBy_NAME = ListDeviceFleetsSortBy' "NAME"

{-# COMPLETE
  ListDeviceFleetsSortBy_CREATION_TIME,
  ListDeviceFleetsSortBy_LAST_MODIFIED_TIME,
  ListDeviceFleetsSortBy_NAME,
  ListDeviceFleetsSortBy'
  #-}
