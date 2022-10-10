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
-- Module      : Amazonka.Rekognition.Types.DatasetStatusMessageCode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.DatasetStatusMessageCode
  ( DatasetStatusMessageCode
      ( ..,
        DatasetStatusMessageCode_CLIENT_ERROR,
        DatasetStatusMessageCode_SERVICE_ERROR,
        DatasetStatusMessageCode_SUCCESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype DatasetStatusMessageCode = DatasetStatusMessageCode'
  { fromDatasetStatusMessageCode ::
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

pattern DatasetStatusMessageCode_CLIENT_ERROR :: DatasetStatusMessageCode
pattern DatasetStatusMessageCode_CLIENT_ERROR = DatasetStatusMessageCode' "CLIENT_ERROR"

pattern DatasetStatusMessageCode_SERVICE_ERROR :: DatasetStatusMessageCode
pattern DatasetStatusMessageCode_SERVICE_ERROR = DatasetStatusMessageCode' "SERVICE_ERROR"

pattern DatasetStatusMessageCode_SUCCESS :: DatasetStatusMessageCode
pattern DatasetStatusMessageCode_SUCCESS = DatasetStatusMessageCode' "SUCCESS"

{-# COMPLETE
  DatasetStatusMessageCode_CLIENT_ERROR,
  DatasetStatusMessageCode_SERVICE_ERROR,
  DatasetStatusMessageCode_SUCCESS,
  DatasetStatusMessageCode'
  #-}
