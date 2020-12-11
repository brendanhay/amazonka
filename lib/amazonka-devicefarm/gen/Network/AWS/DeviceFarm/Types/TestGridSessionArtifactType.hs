-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.TestGridSessionArtifactType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.TestGridSessionArtifactType
  ( TestGridSessionArtifactType
      ( TestGridSessionArtifactType',
        SeleniumLog,
        Unknown,
        Video
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype TestGridSessionArtifactType = TestGridSessionArtifactType' Lude.Text
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

pattern SeleniumLog :: TestGridSessionArtifactType
pattern SeleniumLog = TestGridSessionArtifactType' "SELENIUM_LOG"

pattern Unknown :: TestGridSessionArtifactType
pattern Unknown = TestGridSessionArtifactType' "UNKNOWN"

pattern Video :: TestGridSessionArtifactType
pattern Video = TestGridSessionArtifactType' "VIDEO"

{-# COMPLETE
  SeleniumLog,
  Unknown,
  Video,
  TestGridSessionArtifactType'
  #-}
