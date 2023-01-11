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
-- Module      : Amazonka.Comprehend.Types.AugmentedManifestsDocumentTypeFormat
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.AugmentedManifestsDocumentTypeFormat
  ( AugmentedManifestsDocumentTypeFormat
      ( ..,
        AugmentedManifestsDocumentTypeFormat_PLAIN_TEXT_DOCUMENT,
        AugmentedManifestsDocumentTypeFormat_SEMI_STRUCTURED_DOCUMENT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AugmentedManifestsDocumentTypeFormat = AugmentedManifestsDocumentTypeFormat'
  { fromAugmentedManifestsDocumentTypeFormat ::
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

pattern AugmentedManifestsDocumentTypeFormat_PLAIN_TEXT_DOCUMENT :: AugmentedManifestsDocumentTypeFormat
pattern AugmentedManifestsDocumentTypeFormat_PLAIN_TEXT_DOCUMENT = AugmentedManifestsDocumentTypeFormat' "PLAIN_TEXT_DOCUMENT"

pattern AugmentedManifestsDocumentTypeFormat_SEMI_STRUCTURED_DOCUMENT :: AugmentedManifestsDocumentTypeFormat
pattern AugmentedManifestsDocumentTypeFormat_SEMI_STRUCTURED_DOCUMENT = AugmentedManifestsDocumentTypeFormat' "SEMI_STRUCTURED_DOCUMENT"

{-# COMPLETE
  AugmentedManifestsDocumentTypeFormat_PLAIN_TEXT_DOCUMENT,
  AugmentedManifestsDocumentTypeFormat_SEMI_STRUCTURED_DOCUMENT,
  AugmentedManifestsDocumentTypeFormat'
  #-}
