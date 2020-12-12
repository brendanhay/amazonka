{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProcessingS3UploadMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProcessingS3UploadMode
  ( ProcessingS3UploadMode
      ( ProcessingS3UploadMode',
        Continuous,
        EndOfJob
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ProcessingS3UploadMode = ProcessingS3UploadMode' Lude.Text
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

pattern Continuous :: ProcessingS3UploadMode
pattern Continuous = ProcessingS3UploadMode' "Continuous"

pattern EndOfJob :: ProcessingS3UploadMode
pattern EndOfJob = ProcessingS3UploadMode' "EndOfJob"

{-# COMPLETE
  Continuous,
  EndOfJob,
  ProcessingS3UploadMode'
  #-}
