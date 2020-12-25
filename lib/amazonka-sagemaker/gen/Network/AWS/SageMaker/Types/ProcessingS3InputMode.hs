{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProcessingS3InputMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProcessingS3InputMode
  ( ProcessingS3InputMode
      ( ProcessingS3InputMode',
        ProcessingS3InputModePipe,
        ProcessingS3InputModeFile,
        fromProcessingS3InputMode
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ProcessingS3InputMode = ProcessingS3InputMode'
  { fromProcessingS3InputMode ::
      Core.Text
  }
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

pattern ProcessingS3InputModePipe :: ProcessingS3InputMode
pattern ProcessingS3InputModePipe = ProcessingS3InputMode' "Pipe"

pattern ProcessingS3InputModeFile :: ProcessingS3InputMode
pattern ProcessingS3InputModeFile = ProcessingS3InputMode' "File"

{-# COMPLETE
  ProcessingS3InputModePipe,
  ProcessingS3InputModeFile,
  ProcessingS3InputMode'
  #-}
