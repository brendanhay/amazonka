-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FpgaImageStateCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FpgaImageStateCode
  ( FpgaImageStateCode
      ( FpgaImageStateCode',
        FISCAvailable,
        FISCFailed,
        FISCPending,
        FISCUnavailable
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype FpgaImageStateCode = FpgaImageStateCode' Lude.Text
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

pattern FISCAvailable :: FpgaImageStateCode
pattern FISCAvailable = FpgaImageStateCode' "available"

pattern FISCFailed :: FpgaImageStateCode
pattern FISCFailed = FpgaImageStateCode' "failed"

pattern FISCPending :: FpgaImageStateCode
pattern FISCPending = FpgaImageStateCode' "pending"

pattern FISCUnavailable :: FpgaImageStateCode
pattern FISCUnavailable = FpgaImageStateCode' "unavailable"

{-# COMPLETE
  FISCAvailable,
  FISCFailed,
  FISCPending,
  FISCUnavailable,
  FpgaImageStateCode'
  #-}
