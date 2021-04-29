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
-- Module      : Network.AWS.SageMaker.Types.ContentClassifier
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ContentClassifier
  ( ContentClassifier
      ( ..,
        ContentClassifier_FreeOfAdultContent,
        ContentClassifier_FreeOfPersonallyIdentifiableInformation
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ContentClassifier = ContentClassifier'
  { fromContentClassifier ::
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

pattern ContentClassifier_FreeOfAdultContent :: ContentClassifier
pattern ContentClassifier_FreeOfAdultContent = ContentClassifier' "FreeOfAdultContent"

pattern ContentClassifier_FreeOfPersonallyIdentifiableInformation :: ContentClassifier
pattern ContentClassifier_FreeOfPersonallyIdentifiableInformation = ContentClassifier' "FreeOfPersonallyIdentifiableInformation"

{-# COMPLETE
  ContentClassifier_FreeOfAdultContent,
  ContentClassifier_FreeOfPersonallyIdentifiableInformation,
  ContentClassifier'
  #-}
