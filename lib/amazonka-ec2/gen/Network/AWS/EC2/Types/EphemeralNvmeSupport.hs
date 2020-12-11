-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.EphemeralNvmeSupport
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.EphemeralNvmeSupport
  ( EphemeralNvmeSupport
      ( EphemeralNvmeSupport',
        ERequired,
        ESupported,
        EUnsupported
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype EphemeralNvmeSupport = EphemeralNvmeSupport' Lude.Text
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

pattern ERequired :: EphemeralNvmeSupport
pattern ERequired = EphemeralNvmeSupport' "required"

pattern ESupported :: EphemeralNvmeSupport
pattern ESupported = EphemeralNvmeSupport' "supported"

pattern EUnsupported :: EphemeralNvmeSupport
pattern EUnsupported = EphemeralNvmeSupport' "unsupported"

{-# COMPLETE
  ERequired,
  ESupported,
  EUnsupported,
  EphemeralNvmeSupport'
  #-}
