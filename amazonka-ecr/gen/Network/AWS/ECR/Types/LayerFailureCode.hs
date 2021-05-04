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
-- Module      : Network.AWS.ECR.Types.LayerFailureCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.LayerFailureCode
  ( LayerFailureCode
      ( ..,
        LayerFailureCode_InvalidLayerDigest,
        LayerFailureCode_MissingLayerDigest
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype LayerFailureCode = LayerFailureCode'
  { fromLayerFailureCode ::
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

pattern LayerFailureCode_InvalidLayerDigest :: LayerFailureCode
pattern LayerFailureCode_InvalidLayerDigest = LayerFailureCode' "InvalidLayerDigest"

pattern LayerFailureCode_MissingLayerDigest :: LayerFailureCode
pattern LayerFailureCode_MissingLayerDigest = LayerFailureCode' "MissingLayerDigest"

{-# COMPLETE
  LayerFailureCode_InvalidLayerDigest,
  LayerFailureCode_MissingLayerDigest,
  LayerFailureCode'
  #-}
