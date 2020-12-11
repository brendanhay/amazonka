-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.SoftwareToUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.SoftwareToUpdate
  ( SoftwareToUpdate
      ( SoftwareToUpdate',
        Core,
        OtaAgent
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | The piece of software on the Greengrass core that will be updated.
newtype SoftwareToUpdate = SoftwareToUpdate' Lude.Text
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

pattern Core :: SoftwareToUpdate
pattern Core = SoftwareToUpdate' "core"

pattern OtaAgent :: SoftwareToUpdate
pattern OtaAgent = SoftwareToUpdate' "ota_agent"

{-# COMPLETE
  Core,
  OtaAgent,
  SoftwareToUpdate'
  #-}
