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
-- Module      : Amazonka.MediaConvert.Types.RenewalType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.RenewalType
  ( RenewalType
      ( ..,
        RenewalType_AUTO_RENEW,
        RenewalType_EXPIRE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies whether the term of your reserved queue pricing plan is
-- automatically extended (AUTO_RENEW) or expires (EXPIRE) at the end of
-- the term.
newtype RenewalType = RenewalType'
  { fromRenewalType ::
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

pattern RenewalType_AUTO_RENEW :: RenewalType
pattern RenewalType_AUTO_RENEW = RenewalType' "AUTO_RENEW"

pattern RenewalType_EXPIRE :: RenewalType
pattern RenewalType_EXPIRE = RenewalType' "EXPIRE"

{-# COMPLETE
  RenewalType_AUTO_RENEW,
  RenewalType_EXPIRE,
  RenewalType'
  #-}
