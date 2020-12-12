{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDBStreams.Types.StreamStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDBStreams.Types.StreamStatus
  ( StreamStatus
      ( StreamStatus',
        Disabled,
        Disabling,
        Enabled,
        Enabling
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype StreamStatus = StreamStatus' Lude.Text
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

pattern Disabled :: StreamStatus
pattern Disabled = StreamStatus' "DISABLED"

pattern Disabling :: StreamStatus
pattern Disabling = StreamStatus' "DISABLING"

pattern Enabled :: StreamStatus
pattern Enabled = StreamStatus' "ENABLED"

pattern Enabling :: StreamStatus
pattern Enabling = StreamStatus' "ENABLING"

{-# COMPLETE
  Disabled,
  Disabling,
  Enabled,
  Enabling,
  StreamStatus'
  #-}
