{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.TreatMissingData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.TreatMissingData
  ( TreatMissingData
      ( TreatMissingData',
        Breaching,
        NotBreaching,
        Ignore,
        Missing
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype TreatMissingData = TreatMissingData' Lude.Text
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

pattern Breaching :: TreatMissingData
pattern Breaching = TreatMissingData' "breaching"

pattern NotBreaching :: TreatMissingData
pattern NotBreaching = TreatMissingData' "notBreaching"

pattern Ignore :: TreatMissingData
pattern Ignore = TreatMissingData' "ignore"

pattern Missing :: TreatMissingData
pattern Missing = TreatMissingData' "missing"

{-# COMPLETE
  Breaching,
  NotBreaching,
  Ignore,
  Missing,
  TreatMissingData'
  #-}
