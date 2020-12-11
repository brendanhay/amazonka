-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.HTTPEndpointS3BackupMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.HTTPEndpointS3BackupMode
  ( HTTPEndpointS3BackupMode
      ( HTTPEndpointS3BackupMode',
        AllData,
        FailedDataOnly
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype HTTPEndpointS3BackupMode = HTTPEndpointS3BackupMode' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern AllData :: HTTPEndpointS3BackupMode
pattern AllData = HTTPEndpointS3BackupMode' "AllData"

pattern FailedDataOnly :: HTTPEndpointS3BackupMode
pattern FailedDataOnly = HTTPEndpointS3BackupMode' "FailedDataOnly"

{-# COMPLETE
  AllData,
  FailedDataOnly,
  HTTPEndpointS3BackupMode'
  #-}
