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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
import qualified Amazonka.Prelude as Prelude

newtype ListDeviceFleetsSortBy = ListDeviceFleetsSortBy'
  { fromListDeviceFleetsSortBy ::
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
