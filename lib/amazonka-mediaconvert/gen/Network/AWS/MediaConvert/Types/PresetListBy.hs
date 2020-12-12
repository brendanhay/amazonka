{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.PresetListBy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.PresetListBy
  ( PresetListBy
      ( PresetListBy',
        PLBCreationDate,
        PLBName,
        PLBSystem
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Optional. When you request a list of presets, you can choose to list them alphabetically by NAME or chronologically by CREATION_DATE. If you don't specify, the service will list them by name.
newtype PresetListBy = PresetListBy' Lude.Text
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

pattern PLBCreationDate :: PresetListBy
pattern PLBCreationDate = PresetListBy' "CREATION_DATE"

pattern PLBName :: PresetListBy
pattern PLBName = PresetListBy' "NAME"

pattern PLBSystem :: PresetListBy
pattern PLBSystem = PresetListBy' "SYSTEM"

{-# COMPLETE
  PLBCreationDate,
  PLBName,
  PLBSystem,
  PresetListBy'
  #-}
