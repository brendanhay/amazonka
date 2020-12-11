-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputSourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputSourceType
  ( InputSourceType
      ( InputSourceType',
        ISTDynamic,
        ISTStatic
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | There are two types of input sources, static and dynamic. If an input source is dynamic you can
--
-- change the source url of the input dynamically using an input switch action. However, the only input type
-- to support a dynamic url at this time is MP4_FILE. By default all input sources are static.
newtype InputSourceType = InputSourceType' Lude.Text
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

pattern ISTDynamic :: InputSourceType
pattern ISTDynamic = InputSourceType' "DYNAMIC"

pattern ISTStatic :: InputSourceType
pattern ISTStatic = InputSourceType' "STATIC"

{-# COMPLETE
  ISTDynamic,
  ISTStatic,
  InputSourceType'
  #-}
