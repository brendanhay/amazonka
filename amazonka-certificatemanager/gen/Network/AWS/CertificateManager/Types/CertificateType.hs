{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types.CertificateType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManager.Types.CertificateType
  ( CertificateType
      ( ..,
        CertificateType_AMAZON_ISSUED,
        CertificateType_IMPORTED,
        CertificateType_PRIVATE
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype CertificateType = CertificateType'
  { fromCertificateType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern CertificateType_AMAZON_ISSUED :: CertificateType
pattern CertificateType_AMAZON_ISSUED = CertificateType' "AMAZON_ISSUED"

pattern CertificateType_IMPORTED :: CertificateType
pattern CertificateType_IMPORTED = CertificateType' "IMPORTED"

pattern CertificateType_PRIVATE :: CertificateType
pattern CertificateType_PRIVATE = CertificateType' "PRIVATE"

{-# COMPLETE
  CertificateType_AMAZON_ISSUED,
  CertificateType_IMPORTED,
  CertificateType_PRIVATE,
  CertificateType'
  #-}
