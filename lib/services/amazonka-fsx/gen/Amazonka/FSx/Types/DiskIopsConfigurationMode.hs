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
-- Module      : Amazonka.FSx.Types.DiskIopsConfigurationMode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.DiskIopsConfigurationMode
  ( DiskIopsConfigurationMode
      ( ..,
        DiskIopsConfigurationMode_AUTOMATIC,
        DiskIopsConfigurationMode_USER_PROVISIONED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DiskIopsConfigurationMode = DiskIopsConfigurationMode'
  { fromDiskIopsConfigurationMode ::
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

pattern DiskIopsConfigurationMode_AUTOMATIC :: DiskIopsConfigurationMode
pattern DiskIopsConfigurationMode_AUTOMATIC = DiskIopsConfigurationMode' "AUTOMATIC"

pattern DiskIopsConfigurationMode_USER_PROVISIONED :: DiskIopsConfigurationMode
pattern DiskIopsConfigurationMode_USER_PROVISIONED = DiskIopsConfigurationMode' "USER_PROVISIONED"

{-# COMPLETE
  DiskIopsConfigurationMode_AUTOMATIC,
  DiskIopsConfigurationMode_USER_PROVISIONED,
  DiskIopsConfigurationMode'
  #-}
