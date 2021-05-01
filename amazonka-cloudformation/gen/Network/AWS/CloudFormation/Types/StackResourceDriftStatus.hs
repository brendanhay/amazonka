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
-- Module      : Network.AWS.CloudFormation.Types.StackResourceDriftStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackResourceDriftStatus
  ( StackResourceDriftStatus
      ( ..,
        StackResourceDriftStatus_DELETED,
        StackResourceDriftStatus_IN_SYNC,
        StackResourceDriftStatus_MODIFIED,
        StackResourceDriftStatus_NOT_CHECKED
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype StackResourceDriftStatus = StackResourceDriftStatus'
  { fromStackResourceDriftStatus ::
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

pattern StackResourceDriftStatus_DELETED :: StackResourceDriftStatus
pattern StackResourceDriftStatus_DELETED = StackResourceDriftStatus' "DELETED"

pattern StackResourceDriftStatus_IN_SYNC :: StackResourceDriftStatus
pattern StackResourceDriftStatus_IN_SYNC = StackResourceDriftStatus' "IN_SYNC"

pattern StackResourceDriftStatus_MODIFIED :: StackResourceDriftStatus
pattern StackResourceDriftStatus_MODIFIED = StackResourceDriftStatus' "MODIFIED"

pattern StackResourceDriftStatus_NOT_CHECKED :: StackResourceDriftStatus
pattern StackResourceDriftStatus_NOT_CHECKED = StackResourceDriftStatus' "NOT_CHECKED"

{-# COMPLETE
  StackResourceDriftStatus_DELETED,
  StackResourceDriftStatus_IN_SYNC,
  StackResourceDriftStatus_MODIFIED,
  StackResourceDriftStatus_NOT_CHECKED,
  StackResourceDriftStatus'
  #-}
