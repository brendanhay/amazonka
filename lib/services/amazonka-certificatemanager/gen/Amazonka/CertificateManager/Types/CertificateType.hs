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
-- Module      : Amazonka.CertificateManager.Types.CertificateType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CertificateManager.Types.CertificateType
  ( CertificateType
      ( ..,
        CertificateType_AMAZON_ISSUED,
        CertificateType_IMPORTED,
        CertificateType_PRIVATE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CertificateType = CertificateType'
  { fromCertificateType ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
