{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CertificateSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CertificateSource
  ( CertificateSource
      ( ..,
        CertificateSource_Acm,
        CertificateSource_Cloudfront,
        CertificateSource_Iam
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype CertificateSource = CertificateSource'
  { fromCertificateSource ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern CertificateSource_Acm :: CertificateSource
pattern CertificateSource_Acm = CertificateSource' "acm"

pattern CertificateSource_Cloudfront :: CertificateSource
pattern CertificateSource_Cloudfront = CertificateSource' "cloudfront"

pattern CertificateSource_Iam :: CertificateSource
pattern CertificateSource_Iam = CertificateSource' "iam"

{-# COMPLETE
  CertificateSource_Acm,
  CertificateSource_Cloudfront,
  CertificateSource_Iam,
  CertificateSource'
  #-}
