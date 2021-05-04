{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.FunctionIsolationMode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.FunctionIsolationMode
  ( FunctionIsolationMode
      ( ..,
        FunctionIsolationMode_GreengrassContainer,
        FunctionIsolationMode_NoContainer
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Specifies whether the Lambda function runs in a Greengrass container
-- (default) or without containerization. Unless your scenario requires
-- that you run without containerization, we recommend that you run in a
-- Greengrass container. Omit this value to run the Lambda function with
-- the default containerization for the group.
newtype FunctionIsolationMode = FunctionIsolationMode'
  { fromFunctionIsolationMode ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern FunctionIsolationMode_GreengrassContainer :: FunctionIsolationMode
pattern FunctionIsolationMode_GreengrassContainer = FunctionIsolationMode' "GreengrassContainer"

pattern FunctionIsolationMode_NoContainer :: FunctionIsolationMode
pattern FunctionIsolationMode_NoContainer = FunctionIsolationMode' "NoContainer"

{-# COMPLETE
  FunctionIsolationMode_GreengrassContainer,
  FunctionIsolationMode_NoContainer,
  FunctionIsolationMode'
  #-}
