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
-- Module      : Amazonka.CloudFront.Types.MinimumProtocolVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.MinimumProtocolVersion
  ( MinimumProtocolVersion
      ( ..,
        MinimumProtocolVersion_SSLv3,
        MinimumProtocolVersion_TLSv1,
        MinimumProtocolVersion_TLSv1_1_2016,
        MinimumProtocolVersion_TLSv1_2016,
        MinimumProtocolVersion_TLSv1_2_2018,
        MinimumProtocolVersion_TLSv1_2_2019,
        MinimumProtocolVersion_TLSv1_2_2021
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype MinimumProtocolVersion = MinimumProtocolVersion'
  { fromMinimumProtocolVersion ::
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

pattern MinimumProtocolVersion_SSLv3 :: MinimumProtocolVersion
pattern MinimumProtocolVersion_SSLv3 = MinimumProtocolVersion' "SSLv3"

pattern MinimumProtocolVersion_TLSv1 :: MinimumProtocolVersion
pattern MinimumProtocolVersion_TLSv1 = MinimumProtocolVersion' "TLSv1"

pattern MinimumProtocolVersion_TLSv1_1_2016 :: MinimumProtocolVersion
pattern MinimumProtocolVersion_TLSv1_1_2016 = MinimumProtocolVersion' "TLSv1.1_2016"

pattern MinimumProtocolVersion_TLSv1_2016 :: MinimumProtocolVersion
pattern MinimumProtocolVersion_TLSv1_2016 = MinimumProtocolVersion' "TLSv1_2016"

pattern MinimumProtocolVersion_TLSv1_2_2018 :: MinimumProtocolVersion
pattern MinimumProtocolVersion_TLSv1_2_2018 = MinimumProtocolVersion' "TLSv1.2_2018"

pattern MinimumProtocolVersion_TLSv1_2_2019 :: MinimumProtocolVersion
pattern MinimumProtocolVersion_TLSv1_2_2019 = MinimumProtocolVersion' "TLSv1.2_2019"

pattern MinimumProtocolVersion_TLSv1_2_2021 :: MinimumProtocolVersion
pattern MinimumProtocolVersion_TLSv1_2_2021 = MinimumProtocolVersion' "TLSv1.2_2021"

{-# COMPLETE
  MinimumProtocolVersion_SSLv3,
  MinimumProtocolVersion_TLSv1,
  MinimumProtocolVersion_TLSv1_1_2016,
  MinimumProtocolVersion_TLSv1_2016,
  MinimumProtocolVersion_TLSv1_2_2018,
  MinimumProtocolVersion_TLSv1_2_2019,
  MinimumProtocolVersion_TLSv1_2_2021,
  MinimumProtocolVersion'
  #-}
