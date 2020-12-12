{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.JobPhase
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.JobPhase
  ( JobPhase
      ( JobPhase',
        Probing,
        Transcoding,
        Uploading
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | A job's phase can be PROBING, TRANSCODING OR UPLOADING
newtype JobPhase = JobPhase' Lude.Text
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

pattern Probing :: JobPhase
pattern Probing = JobPhase' "PROBING"

pattern Transcoding :: JobPhase
pattern Transcoding = JobPhase' "TRANSCODING"

pattern Uploading :: JobPhase
pattern Uploading = JobPhase' "UPLOADING"

{-# COMPLETE
  Probing,
  Transcoding,
  Uploading,
  JobPhase'
  #-}
