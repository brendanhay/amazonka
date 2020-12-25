{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.SchemaStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.SchemaStatus
  ( SchemaStatus
      ( SchemaStatus',
        SchemaStatusProcessing,
        SchemaStatusActive,
        SchemaStatusDeleting,
        SchemaStatusFailed,
        SchemaStatusSuccess,
        SchemaStatusNotApplicable,
        fromSchemaStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype SchemaStatus = SchemaStatus' {fromSchemaStatus :: Core.Text}
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern SchemaStatusProcessing :: SchemaStatus
pattern SchemaStatusProcessing = SchemaStatus' "PROCESSING"

pattern SchemaStatusActive :: SchemaStatus
pattern SchemaStatusActive = SchemaStatus' "ACTIVE"

pattern SchemaStatusDeleting :: SchemaStatus
pattern SchemaStatusDeleting = SchemaStatus' "DELETING"

pattern SchemaStatusFailed :: SchemaStatus
pattern SchemaStatusFailed = SchemaStatus' "FAILED"

pattern SchemaStatusSuccess :: SchemaStatus
pattern SchemaStatusSuccess = SchemaStatus' "SUCCESS"

pattern SchemaStatusNotApplicable :: SchemaStatus
pattern SchemaStatusNotApplicable = SchemaStatus' "NOT_APPLICABLE"

{-# COMPLETE
  SchemaStatusProcessing,
  SchemaStatusActive,
  SchemaStatusDeleting,
  SchemaStatusFailed,
  SchemaStatusSuccess,
  SchemaStatusNotApplicable,
  SchemaStatus'
  #-}
