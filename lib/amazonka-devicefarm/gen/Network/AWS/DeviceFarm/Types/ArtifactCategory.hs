-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.ArtifactCategory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.ArtifactCategory
  ( ArtifactCategory
      ( ArtifactCategory',
        File,
        Log,
        Screenshot
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ArtifactCategory = ArtifactCategory' Lude.Text
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

pattern File :: ArtifactCategory
pattern File = ArtifactCategory' "FILE"

pattern Log :: ArtifactCategory
pattern Log = ArtifactCategory' "LOG"

pattern Screenshot :: ArtifactCategory
pattern Screenshot = ArtifactCategory' "SCREENSHOT"

{-# COMPLETE
  File,
  Log,
  Screenshot,
  ArtifactCategory'
  #-}
