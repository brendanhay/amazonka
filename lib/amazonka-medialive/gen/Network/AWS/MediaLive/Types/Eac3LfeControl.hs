-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Eac3LfeControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Eac3LfeControl
  ( Eac3LfeControl
      ( Eac3LfeControl',
        Lfe,
        NoLfe
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Eac3 Lfe Control
newtype Eac3LfeControl = Eac3LfeControl' Lude.Text
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

pattern Lfe :: Eac3LfeControl
pattern Lfe = Eac3LfeControl' "LFE"

pattern NoLfe :: Eac3LfeControl
pattern NoLfe = Eac3LfeControl' "NO_LFE"

{-# COMPLETE
  Lfe,
  NoLfe,
  Eac3LfeControl'
  #-}
