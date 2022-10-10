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
-- Module      : Amazonka.DataSync.Types.HdfsRpcProtection
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataSync.Types.HdfsRpcProtection
  ( HdfsRpcProtection
      ( ..,
        HdfsRpcProtection_AUTHENTICATION,
        HdfsRpcProtection_DISABLED,
        HdfsRpcProtection_INTEGRITY,
        HdfsRpcProtection_PRIVACY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype HdfsRpcProtection = HdfsRpcProtection'
  { fromHdfsRpcProtection ::
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

pattern HdfsRpcProtection_AUTHENTICATION :: HdfsRpcProtection
pattern HdfsRpcProtection_AUTHENTICATION = HdfsRpcProtection' "AUTHENTICATION"

pattern HdfsRpcProtection_DISABLED :: HdfsRpcProtection
pattern HdfsRpcProtection_DISABLED = HdfsRpcProtection' "DISABLED"

pattern HdfsRpcProtection_INTEGRITY :: HdfsRpcProtection
pattern HdfsRpcProtection_INTEGRITY = HdfsRpcProtection' "INTEGRITY"

pattern HdfsRpcProtection_PRIVACY :: HdfsRpcProtection
pattern HdfsRpcProtection_PRIVACY = HdfsRpcProtection' "PRIVACY"

{-# COMPLETE
  HdfsRpcProtection_AUTHENTICATION,
  HdfsRpcProtection_DISABLED,
  HdfsRpcProtection_INTEGRITY,
  HdfsRpcProtection_PRIVACY,
  HdfsRpcProtection'
  #-}
