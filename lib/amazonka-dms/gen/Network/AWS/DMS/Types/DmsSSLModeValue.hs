{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.DmsSSLModeValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.DmsSSLModeValue
  ( DmsSSLModeValue
      ( DmsSSLModeValue',
        DSMVNone,
        DSMVRequire,
        DSMVVerifyCa,
        DSMVVerifyFull
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype DmsSSLModeValue = DmsSSLModeValue' Lude.Text
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

pattern DSMVNone :: DmsSSLModeValue
pattern DSMVNone = DmsSSLModeValue' "none"

pattern DSMVRequire :: DmsSSLModeValue
pattern DSMVRequire = DmsSSLModeValue' "require"

pattern DSMVVerifyCa :: DmsSSLModeValue
pattern DSMVVerifyCa = DmsSSLModeValue' "verify-ca"

pattern DSMVVerifyFull :: DmsSSLModeValue
pattern DSMVVerifyFull = DmsSSLModeValue' "verify-full"

{-# COMPLETE
  DSMVNone,
  DSMVRequire,
  DSMVVerifyCa,
  DSMVVerifyFull,
  DmsSSLModeValue'
  #-}
