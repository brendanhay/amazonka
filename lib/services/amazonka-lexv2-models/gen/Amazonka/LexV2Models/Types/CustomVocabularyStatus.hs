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
-- Module      : Amazonka.LexV2Models.Types.CustomVocabularyStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.CustomVocabularyStatus
  ( CustomVocabularyStatus
      ( ..,
        CustomVocabularyStatus_Creating,
        CustomVocabularyStatus_Deleting,
        CustomVocabularyStatus_Exporting,
        CustomVocabularyStatus_Importing,
        CustomVocabularyStatus_Ready
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CustomVocabularyStatus = CustomVocabularyStatus'
  { fromCustomVocabularyStatus ::
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

pattern CustomVocabularyStatus_Creating :: CustomVocabularyStatus
pattern CustomVocabularyStatus_Creating = CustomVocabularyStatus' "Creating"

pattern CustomVocabularyStatus_Deleting :: CustomVocabularyStatus
pattern CustomVocabularyStatus_Deleting = CustomVocabularyStatus' "Deleting"

pattern CustomVocabularyStatus_Exporting :: CustomVocabularyStatus
pattern CustomVocabularyStatus_Exporting = CustomVocabularyStatus' "Exporting"

pattern CustomVocabularyStatus_Importing :: CustomVocabularyStatus
pattern CustomVocabularyStatus_Importing = CustomVocabularyStatus' "Importing"

pattern CustomVocabularyStatus_Ready :: CustomVocabularyStatus
pattern CustomVocabularyStatus_Ready = CustomVocabularyStatus' "Ready"

{-# COMPLETE
  CustomVocabularyStatus_Creating,
  CustomVocabularyStatus_Deleting,
  CustomVocabularyStatus_Exporting,
  CustomVocabularyStatus_Importing,
  CustomVocabularyStatus_Ready,
  CustomVocabularyStatus'
  #-}
