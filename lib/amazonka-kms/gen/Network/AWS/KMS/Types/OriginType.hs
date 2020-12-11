-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.OriginType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.OriginType
  ( OriginType
      ( OriginType',
        AWSCloudhsm,
        AWSKMS,
        External
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype OriginType = OriginType' Lude.Text
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

pattern AWSCloudhsm :: OriginType
pattern AWSCloudhsm = OriginType' "AWS_CLOUDHSM"

pattern AWSKMS :: OriginType
pattern AWSKMS = OriginType' "AWS_KMS"

pattern External :: OriginType
pattern External = OriginType' "EXTERNAL"

{-# COMPLETE
  AWSCloudhsm,
  AWSKMS,
  External,
  OriginType'
  #-}
