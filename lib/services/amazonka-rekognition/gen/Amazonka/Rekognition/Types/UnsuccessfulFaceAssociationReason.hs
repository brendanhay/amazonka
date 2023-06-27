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
-- Module      : Amazonka.Rekognition.Types.UnsuccessfulFaceAssociationReason
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.UnsuccessfulFaceAssociationReason
  ( UnsuccessfulFaceAssociationReason
      ( ..,
        UnsuccessfulFaceAssociationReason_ASSOCIATED_TO_A_DIFFERENT_USER,
        UnsuccessfulFaceAssociationReason_FACE_NOT_FOUND,
        UnsuccessfulFaceAssociationReason_LOW_MATCH_CONFIDENCE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype UnsuccessfulFaceAssociationReason = UnsuccessfulFaceAssociationReason'
  { fromUnsuccessfulFaceAssociationReason ::
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

pattern UnsuccessfulFaceAssociationReason_ASSOCIATED_TO_A_DIFFERENT_USER :: UnsuccessfulFaceAssociationReason
pattern UnsuccessfulFaceAssociationReason_ASSOCIATED_TO_A_DIFFERENT_USER = UnsuccessfulFaceAssociationReason' "ASSOCIATED_TO_A_DIFFERENT_USER"

pattern UnsuccessfulFaceAssociationReason_FACE_NOT_FOUND :: UnsuccessfulFaceAssociationReason
pattern UnsuccessfulFaceAssociationReason_FACE_NOT_FOUND = UnsuccessfulFaceAssociationReason' "FACE_NOT_FOUND"

pattern UnsuccessfulFaceAssociationReason_LOW_MATCH_CONFIDENCE :: UnsuccessfulFaceAssociationReason
pattern UnsuccessfulFaceAssociationReason_LOW_MATCH_CONFIDENCE = UnsuccessfulFaceAssociationReason' "LOW_MATCH_CONFIDENCE"

{-# COMPLETE
  UnsuccessfulFaceAssociationReason_ASSOCIATED_TO_A_DIFFERENT_USER,
  UnsuccessfulFaceAssociationReason_FACE_NOT_FOUND,
  UnsuccessfulFaceAssociationReason_LOW_MATCH_CONFIDENCE,
  UnsuccessfulFaceAssociationReason'
  #-}
