{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ListDeviceFleetsSortBy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ListDeviceFleetsSortBy
  ( ListDeviceFleetsSortBy
      ( ..,
        ListDeviceFleetsSortBy_CREATION_TIME,
        ListDeviceFleetsSortBy_LAST_MODIFIED_TIME,
        ListDeviceFleetsSortBy_NAME
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ListDeviceFleetsSortBy = ListDeviceFleetsSortBy'
  { fromListDeviceFleetsSortBy ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
