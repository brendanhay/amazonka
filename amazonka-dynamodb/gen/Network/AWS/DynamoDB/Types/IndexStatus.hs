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
-- Module      : Network.AWS.DynamoDB.Types.IndexStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.IndexStatus
  ( IndexStatus
      ( ..,
        IndexStatus_ACTIVE,
        IndexStatus_CREATING,
        IndexStatus_DELETING,
        IndexStatus_UPDATING
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype IndexStatus = IndexStatus'
  { fromIndexStatus ::
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

pattern IndexStatus_ACTIVE :: IndexStatus
pattern IndexStatus_ACTIVE = IndexStatus' "ACTIVE"

pattern IndexStatus_CREATING :: IndexStatus
pattern IndexStatus_CREATING = IndexStatus' "CREATING"

pattern IndexStatus_DELETING :: IndexStatus
pattern IndexStatus_DELETING = IndexStatus' "DELETING"

pattern IndexStatus_UPDATING :: IndexStatus
pattern IndexStatus_UPDATING = IndexStatus' "UPDATING"

{-# COMPLETE
  IndexStatus_ACTIVE,
  IndexStatus_CREATING,
  IndexStatus_DELETING,
  IndexStatus_UPDATING,
  IndexStatus'
  #-}
