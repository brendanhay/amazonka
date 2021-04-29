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
-- Module      : Network.AWS.Comprehend.Types.PiiEntitiesDetectionMaskMode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.PiiEntitiesDetectionMaskMode
  ( PiiEntitiesDetectionMaskMode
      ( ..,
        PiiEntitiesDetectionMaskMode_MASK,
        PiiEntitiesDetectionMaskMode_REPLACE_WITH_PII_ENTITY_TYPE
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype PiiEntitiesDetectionMaskMode = PiiEntitiesDetectionMaskMode'
  { fromPiiEntitiesDetectionMaskMode ::
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

pattern PiiEntitiesDetectionMaskMode_MASK :: PiiEntitiesDetectionMaskMode
pattern PiiEntitiesDetectionMaskMode_MASK = PiiEntitiesDetectionMaskMode' "MASK"

pattern PiiEntitiesDetectionMaskMode_REPLACE_WITH_PII_ENTITY_TYPE :: PiiEntitiesDetectionMaskMode
pattern PiiEntitiesDetectionMaskMode_REPLACE_WITH_PII_ENTITY_TYPE = PiiEntitiesDetectionMaskMode' "REPLACE_WITH_PII_ENTITY_TYPE"

{-# COMPLETE
  PiiEntitiesDetectionMaskMode_MASK,
  PiiEntitiesDetectionMaskMode_REPLACE_WITH_PII_ENTITY_TYPE,
  PiiEntitiesDetectionMaskMode'
  #-}
