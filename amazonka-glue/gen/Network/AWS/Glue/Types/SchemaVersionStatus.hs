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
-- Module      : Network.AWS.Glue.Types.SchemaVersionStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.SchemaVersionStatus
  ( SchemaVersionStatus
      ( ..,
        SchemaVersionStatus_AVAILABLE,
        SchemaVersionStatus_DELETING,
        SchemaVersionStatus_FAILURE,
        SchemaVersionStatus_PENDING
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype SchemaVersionStatus = SchemaVersionStatus'
  { fromSchemaVersionStatus ::
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

pattern SchemaVersionStatus_AVAILABLE :: SchemaVersionStatus
pattern SchemaVersionStatus_AVAILABLE = SchemaVersionStatus' "AVAILABLE"

pattern SchemaVersionStatus_DELETING :: SchemaVersionStatus
pattern SchemaVersionStatus_DELETING = SchemaVersionStatus' "DELETING"

pattern SchemaVersionStatus_FAILURE :: SchemaVersionStatus
pattern SchemaVersionStatus_FAILURE = SchemaVersionStatus' "FAILURE"

pattern SchemaVersionStatus_PENDING :: SchemaVersionStatus
pattern SchemaVersionStatus_PENDING = SchemaVersionStatus' "PENDING"

{-# COMPLETE
  SchemaVersionStatus_AVAILABLE,
  SchemaVersionStatus_DELETING,
  SchemaVersionStatus_FAILURE,
  SchemaVersionStatus_PENDING,
  SchemaVersionStatus'
  #-}
