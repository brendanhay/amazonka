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
-- Module      : Amazonka.DirectConnect.Types.NniPartnerType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectConnect.Types.NniPartnerType
  ( NniPartnerType
      ( ..,
        NniPartnerType_NonPartner,
        NniPartnerType_V1,
        NniPartnerType_V2
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype NniPartnerType = NniPartnerType'
  { fromNniPartnerType ::
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

pattern NniPartnerType_NonPartner :: NniPartnerType
pattern NniPartnerType_NonPartner = NniPartnerType' "nonPartner"

pattern NniPartnerType_V1 :: NniPartnerType
pattern NniPartnerType_V1 = NniPartnerType' "v1"

pattern NniPartnerType_V2 :: NniPartnerType
pattern NniPartnerType_V2 = NniPartnerType' "v2"

{-# COMPLETE
  NniPartnerType_NonPartner,
  NniPartnerType_V1,
  NniPartnerType_V2,
  NniPartnerType'
  #-}
