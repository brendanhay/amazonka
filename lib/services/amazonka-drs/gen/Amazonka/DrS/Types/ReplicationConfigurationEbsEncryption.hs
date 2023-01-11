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
-- Module      : Amazonka.DrS.Types.ReplicationConfigurationEbsEncryption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.ReplicationConfigurationEbsEncryption
  ( ReplicationConfigurationEbsEncryption
      ( ..,
        ReplicationConfigurationEbsEncryption_CUSTOM,
        ReplicationConfigurationEbsEncryption_DEFAULT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ReplicationConfigurationEbsEncryption = ReplicationConfigurationEbsEncryption'
  { fromReplicationConfigurationEbsEncryption ::
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

pattern ReplicationConfigurationEbsEncryption_CUSTOM :: ReplicationConfigurationEbsEncryption
pattern ReplicationConfigurationEbsEncryption_CUSTOM = ReplicationConfigurationEbsEncryption' "CUSTOM"

pattern ReplicationConfigurationEbsEncryption_DEFAULT :: ReplicationConfigurationEbsEncryption
pattern ReplicationConfigurationEbsEncryption_DEFAULT = ReplicationConfigurationEbsEncryption' "DEFAULT"

{-# COMPLETE
  ReplicationConfigurationEbsEncryption_CUSTOM,
  ReplicationConfigurationEbsEncryption_DEFAULT,
  ReplicationConfigurationEbsEncryption'
  #-}
