{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.Frequency
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.Frequency
  ( Frequency
      ( Frequency',
        Daily,
        Event,
        Hourly,
        Monthly,
        Once,
        Weekly
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype Frequency = Frequency' Lude.Text
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

pattern Daily :: Frequency
pattern Daily = Frequency' "DAILY"

pattern Event :: Frequency
pattern Event = Frequency' "EVENT"

pattern Hourly :: Frequency
pattern Hourly = Frequency' "HOURLY"

pattern Monthly :: Frequency
pattern Monthly = Frequency' "MONTHLY"

pattern Once :: Frequency
pattern Once = Frequency' "ONCE"

pattern Weekly :: Frequency
pattern Weekly = Frequency' "WEEKLY"

{-# COMPLETE
  Daily,
  Event,
  Hourly,
  Monthly,
  Once,
  Weekly,
  Frequency'
  #-}
