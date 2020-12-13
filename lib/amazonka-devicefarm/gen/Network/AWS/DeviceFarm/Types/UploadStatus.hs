{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.UploadStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.UploadStatus
  ( UploadStatus
      ( UploadStatus',
        Initialized,
        Processing,
        Succeeded,
        Failed
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype UploadStatus = UploadStatus' Lude.Text
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

pattern Initialized :: UploadStatus
pattern Initialized = UploadStatus' "INITIALIZED"

pattern Processing :: UploadStatus
pattern Processing = UploadStatus' "PROCESSING"

pattern Succeeded :: UploadStatus
pattern Succeeded = UploadStatus' "SUCCEEDED"

pattern Failed :: UploadStatus
pattern Failed = UploadStatus' "FAILED"

{-# COMPLETE
  Initialized,
  Processing,
  Succeeded,
  Failed,
  UploadStatus'
  #-}
