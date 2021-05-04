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
-- Module      : Network.AWS.Redshift.Types.TableRestoreStatusType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.TableRestoreStatusType
  ( TableRestoreStatusType
      ( ..,
        TableRestoreStatusType_CANCELED,
        TableRestoreStatusType_FAILED,
        TableRestoreStatusType_IN_PROGRESS,
        TableRestoreStatusType_PENDING,
        TableRestoreStatusType_SUCCEEDED
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Internal

newtype TableRestoreStatusType = TableRestoreStatusType'
  { fromTableRestoreStatusType ::
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

pattern TableRestoreStatusType_CANCELED :: TableRestoreStatusType
pattern TableRestoreStatusType_CANCELED = TableRestoreStatusType' "CANCELED"

pattern TableRestoreStatusType_FAILED :: TableRestoreStatusType
pattern TableRestoreStatusType_FAILED = TableRestoreStatusType' "FAILED"

pattern TableRestoreStatusType_IN_PROGRESS :: TableRestoreStatusType
pattern TableRestoreStatusType_IN_PROGRESS = TableRestoreStatusType' "IN_PROGRESS"

pattern TableRestoreStatusType_PENDING :: TableRestoreStatusType
pattern TableRestoreStatusType_PENDING = TableRestoreStatusType' "PENDING"

pattern TableRestoreStatusType_SUCCEEDED :: TableRestoreStatusType
pattern TableRestoreStatusType_SUCCEEDED = TableRestoreStatusType' "SUCCEEDED"

{-# COMPLETE
  TableRestoreStatusType_CANCELED,
  TableRestoreStatusType_FAILED,
  TableRestoreStatusType_IN_PROGRESS,
  TableRestoreStatusType_PENDING,
  TableRestoreStatusType_SUCCEEDED,
  TableRestoreStatusType'
  #-}
