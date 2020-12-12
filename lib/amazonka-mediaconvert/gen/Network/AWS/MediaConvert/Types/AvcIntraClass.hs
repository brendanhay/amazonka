{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AvcIntraClass
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AvcIntraClass
  ( AvcIntraClass
      ( AvcIntraClass',
        Class100,
        Class200,
        Class50
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Specify the AVC-Intra class of your output. The AVC-Intra class selection determines the output video bit rate depending on the frame rate of the output. Outputs with higher class values have higher bitrates and improved image quality.
newtype AvcIntraClass = AvcIntraClass' Lude.Text
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

pattern Class100 :: AvcIntraClass
pattern Class100 = AvcIntraClass' "CLASS_100"

pattern Class200 :: AvcIntraClass
pattern Class200 = AvcIntraClass' "CLASS_200"

pattern Class50 :: AvcIntraClass
pattern Class50 = AvcIntraClass' "CLASS_50"

{-# COMPLETE
  Class100,
  Class200,
  Class50,
  AvcIntraClass'
  #-}
