-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types.CertificateType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManager.Types.CertificateType
  ( CertificateType
      ( CertificateType',
        AmazonIssued,
        Imported,
        Private
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype CertificateType = CertificateType' Lude.Text
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

pattern AmazonIssued :: CertificateType
pattern AmazonIssued = CertificateType' "AMAZON_ISSUED"

pattern Imported :: CertificateType
pattern Imported = CertificateType' "IMPORTED"

pattern Private :: CertificateType
pattern Private = CertificateType' "PRIVATE"

{-# COMPLETE
  AmazonIssued,
  Imported,
  Private,
  CertificateType'
  #-}
