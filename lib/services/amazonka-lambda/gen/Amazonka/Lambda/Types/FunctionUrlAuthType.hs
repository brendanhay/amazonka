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
-- Module      : Amazonka.Lambda.Types.FunctionUrlAuthType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types.FunctionUrlAuthType
  ( FunctionUrlAuthType
      ( ..,
        FunctionUrlAuthType_AWS_IAM,
        FunctionUrlAuthType_NONE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype FunctionUrlAuthType = FunctionUrlAuthType'
  { fromFunctionUrlAuthType ::
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

pattern FunctionUrlAuthType_AWS_IAM :: FunctionUrlAuthType
pattern FunctionUrlAuthType_AWS_IAM = FunctionUrlAuthType' "AWS_IAM"

pattern FunctionUrlAuthType_NONE :: FunctionUrlAuthType
pattern FunctionUrlAuthType_NONE = FunctionUrlAuthType' "NONE"

{-# COMPLETE
  FunctionUrlAuthType_AWS_IAM,
  FunctionUrlAuthType_NONE,
  FunctionUrlAuthType'
  #-}
