-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.WrappingKeySpec
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.WrappingKeySpec
  ( WrappingKeySpec
      ( WrappingKeySpec',
        Rsa2048
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype WrappingKeySpec = WrappingKeySpec' Lude.Text
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

pattern Rsa2048 :: WrappingKeySpec
pattern Rsa2048 = WrappingKeySpec' "RSA_2048"

{-# COMPLETE
  Rsa2048,
  WrappingKeySpec'
  #-}
