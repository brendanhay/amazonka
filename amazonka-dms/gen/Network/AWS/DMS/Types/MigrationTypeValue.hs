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
-- Module      : Network.AWS.DMS.Types.MigrationTypeValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.MigrationTypeValue
  ( MigrationTypeValue
      ( ..,
        MigrationTypeValue_Cdc,
        MigrationTypeValue_Full_load,
        MigrationTypeValue_Full_load_and_cdc
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype MigrationTypeValue = MigrationTypeValue'
  { fromMigrationTypeValue ::
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

pattern MigrationTypeValue_Cdc :: MigrationTypeValue
pattern MigrationTypeValue_Cdc = MigrationTypeValue' "cdc"

pattern MigrationTypeValue_Full_load :: MigrationTypeValue
pattern MigrationTypeValue_Full_load = MigrationTypeValue' "full-load"

pattern MigrationTypeValue_Full_load_and_cdc :: MigrationTypeValue
pattern MigrationTypeValue_Full_load_and_cdc = MigrationTypeValue' "full-load-and-cdc"

{-# COMPLETE
  MigrationTypeValue_Cdc,
  MigrationTypeValue_Full_load,
  MigrationTypeValue_Full_load_and_cdc,
  MigrationTypeValue'
  #-}
