{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.S3BackupMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.S3BackupMode
  ( S3BackupMode
      ( S3BackupMode',
        S3BackupModeDisabled,
        S3BackupModeEnabled,
        fromS3BackupMode
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype S3BackupMode = S3BackupMode' {fromS3BackupMode :: Core.Text}
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

pattern S3BackupModeDisabled :: S3BackupMode
pattern S3BackupModeDisabled = S3BackupMode' "Disabled"

pattern S3BackupModeEnabled :: S3BackupMode
pattern S3BackupModeEnabled = S3BackupMode' "Enabled"

{-# COMPLETE
  S3BackupModeDisabled,
  S3BackupModeEnabled,
  S3BackupMode'
  #-}
