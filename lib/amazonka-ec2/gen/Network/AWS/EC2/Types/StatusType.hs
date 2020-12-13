{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.StatusType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.StatusType
  ( StatusType
      ( StatusType',
        STPassed,
        STFailed,
        STInsufficientData,
        STInitializing
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype StatusType = StatusType' Lude.Text
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

pattern STPassed :: StatusType
pattern STPassed = StatusType' "passed"

pattern STFailed :: StatusType
pattern STFailed = StatusType' "failed"

pattern STInsufficientData :: StatusType
pattern STInsufficientData = StatusType' "insufficient-data"

pattern STInitializing :: StatusType
pattern STInitializing = StatusType' "initializing"

{-# COMPLETE
  STPassed,
  STFailed,
  STInsufficientData,
  STInitializing,
  StatusType'
  #-}
