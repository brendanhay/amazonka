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
-- Module      : Amazonka.LicenseManager.Types.ActivationOverrideBehavior
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManager.Types.ActivationOverrideBehavior
  ( ActivationOverrideBehavior
      ( ..,
        ActivationOverrideBehavior_ALL_GRANTS_PERMITTED_BY_ISSUER,
        ActivationOverrideBehavior_DISTRIBUTED_GRANTS_ONLY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ActivationOverrideBehavior = ActivationOverrideBehavior'
  { fromActivationOverrideBehavior ::
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

pattern ActivationOverrideBehavior_ALL_GRANTS_PERMITTED_BY_ISSUER :: ActivationOverrideBehavior
pattern ActivationOverrideBehavior_ALL_GRANTS_PERMITTED_BY_ISSUER = ActivationOverrideBehavior' "ALL_GRANTS_PERMITTED_BY_ISSUER"

pattern ActivationOverrideBehavior_DISTRIBUTED_GRANTS_ONLY :: ActivationOverrideBehavior
pattern ActivationOverrideBehavior_DISTRIBUTED_GRANTS_ONLY = ActivationOverrideBehavior' "DISTRIBUTED_GRANTS_ONLY"

{-# COMPLETE
  ActivationOverrideBehavior_ALL_GRANTS_PERMITTED_BY_ISSUER,
  ActivationOverrideBehavior_DISTRIBUTED_GRANTS_ONLY,
  ActivationOverrideBehavior'
  #-}
