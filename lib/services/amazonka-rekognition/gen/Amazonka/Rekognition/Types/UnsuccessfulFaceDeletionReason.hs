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
-- Module      : Amazonka.Rekognition.Types.UnsuccessfulFaceDeletionReason
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.UnsuccessfulFaceDeletionReason
  ( UnsuccessfulFaceDeletionReason
      ( ..,
        UnsuccessfulFaceDeletionReason_ASSOCIATED_TO_AN_EXISTING_USER,
        UnsuccessfulFaceDeletionReason_FACE_NOT_FOUND
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype UnsuccessfulFaceDeletionReason = UnsuccessfulFaceDeletionReason'
  { fromUnsuccessfulFaceDeletionReason ::
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

pattern UnsuccessfulFaceDeletionReason_ASSOCIATED_TO_AN_EXISTING_USER :: UnsuccessfulFaceDeletionReason
pattern UnsuccessfulFaceDeletionReason_ASSOCIATED_TO_AN_EXISTING_USER = UnsuccessfulFaceDeletionReason' "ASSOCIATED_TO_AN_EXISTING_USER"

pattern UnsuccessfulFaceDeletionReason_FACE_NOT_FOUND :: UnsuccessfulFaceDeletionReason
pattern UnsuccessfulFaceDeletionReason_FACE_NOT_FOUND = UnsuccessfulFaceDeletionReason' "FACE_NOT_FOUND"

{-# COMPLETE
  UnsuccessfulFaceDeletionReason_ASSOCIATED_TO_AN_EXISTING_USER,
  UnsuccessfulFaceDeletionReason_FACE_NOT_FOUND,
  UnsuccessfulFaceDeletionReason'
  #-}
