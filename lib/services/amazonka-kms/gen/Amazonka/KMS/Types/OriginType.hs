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
-- Module      : Amazonka.KMS.Types.OriginType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KMS.Types.OriginType
  ( OriginType
      ( ..,
        OriginType_AWS_CLOUDHSM,
        OriginType_AWS_KMS,
        OriginType_EXTERNAL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype OriginType = OriginType'
  { fromOriginType ::
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

pattern OriginType_AWS_CLOUDHSM :: OriginType
pattern OriginType_AWS_CLOUDHSM = OriginType' "AWS_CLOUDHSM"

pattern OriginType_AWS_KMS :: OriginType
pattern OriginType_AWS_KMS = OriginType' "AWS_KMS"

pattern OriginType_EXTERNAL :: OriginType
pattern OriginType_EXTERNAL = OriginType' "EXTERNAL"

{-# COMPLETE
  OriginType_AWS_CLOUDHSM,
  OriginType_AWS_KMS,
  OriginType_EXTERNAL,
  OriginType'
  #-}
