{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.HITAccessActions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.HITAccessActions
  ( HITAccessActions
      ( HITAccessActions',
        Accept,
        PreviewAndAccept,
        DiscoverPreviewAndAccept
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype HITAccessActions = HITAccessActions' Lude.Text
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

pattern Accept :: HITAccessActions
pattern Accept = HITAccessActions' "Accept"

pattern PreviewAndAccept :: HITAccessActions
pattern PreviewAndAccept = HITAccessActions' "PreviewAndAccept"

pattern DiscoverPreviewAndAccept :: HITAccessActions
pattern DiscoverPreviewAndAccept = HITAccessActions' "DiscoverPreviewAndAccept"

{-# COMPLETE
  Accept,
  PreviewAndAccept,
  DiscoverPreviewAndAccept,
  HITAccessActions'
  #-}
