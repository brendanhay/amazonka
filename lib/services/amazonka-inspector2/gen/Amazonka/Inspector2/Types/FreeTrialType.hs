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
-- Module      : Amazonka.Inspector2.Types.FreeTrialType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.FreeTrialType
  ( FreeTrialType
      ( ..,
        FreeTrialType_EC2,
        FreeTrialType_ECR,
        FreeTrialType_LAMBDA,
        FreeTrialType_LAMBDA_CODE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype FreeTrialType = FreeTrialType'
  { fromFreeTrialType ::
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

pattern FreeTrialType_EC2 :: FreeTrialType
pattern FreeTrialType_EC2 = FreeTrialType' "EC2"

pattern FreeTrialType_ECR :: FreeTrialType
pattern FreeTrialType_ECR = FreeTrialType' "ECR"

pattern FreeTrialType_LAMBDA :: FreeTrialType
pattern FreeTrialType_LAMBDA = FreeTrialType' "LAMBDA"

pattern FreeTrialType_LAMBDA_CODE :: FreeTrialType
pattern FreeTrialType_LAMBDA_CODE = FreeTrialType' "LAMBDA_CODE"

{-# COMPLETE
  FreeTrialType_EC2,
  FreeTrialType_ECR,
  FreeTrialType_LAMBDA,
  FreeTrialType_LAMBDA_CODE,
  FreeTrialType'
  #-}
