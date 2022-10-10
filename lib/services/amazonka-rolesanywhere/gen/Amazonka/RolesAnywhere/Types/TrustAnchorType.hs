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
-- Module      : Amazonka.RolesAnywhere.Types.TrustAnchorType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RolesAnywhere.Types.TrustAnchorType
  ( TrustAnchorType
      ( ..,
        TrustAnchorType_AWS_ACM_PCA,
        TrustAnchorType_CERTIFICATE_BUNDLE,
        TrustAnchorType_SELF_SIGNED_REPOSITORY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype TrustAnchorType = TrustAnchorType'
  { fromTrustAnchorType ::
      Core.Text
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

pattern TrustAnchorType_AWS_ACM_PCA :: TrustAnchorType
pattern TrustAnchorType_AWS_ACM_PCA = TrustAnchorType' "AWS_ACM_PCA"

pattern TrustAnchorType_CERTIFICATE_BUNDLE :: TrustAnchorType
pattern TrustAnchorType_CERTIFICATE_BUNDLE = TrustAnchorType' "CERTIFICATE_BUNDLE"

pattern TrustAnchorType_SELF_SIGNED_REPOSITORY :: TrustAnchorType
pattern TrustAnchorType_SELF_SIGNED_REPOSITORY = TrustAnchorType' "SELF_SIGNED_REPOSITORY"

{-# COMPLETE
  TrustAnchorType_AWS_ACM_PCA,
  TrustAnchorType_CERTIFICATE_BUNDLE,
  TrustAnchorType_SELF_SIGNED_REPOSITORY,
  TrustAnchorType'
  #-}
