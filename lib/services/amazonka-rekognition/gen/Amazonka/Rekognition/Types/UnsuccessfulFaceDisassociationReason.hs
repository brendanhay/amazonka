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
-- Module      : Amazonka.Rekognition.Types.UnsuccessfulFaceDisassociationReason
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.UnsuccessfulFaceDisassociationReason
  ( UnsuccessfulFaceDisassociationReason
      ( ..,
        UnsuccessfulFaceDisassociationReason_ASSOCIATED_TO_A_DIFFERENT_USER,
        UnsuccessfulFaceDisassociationReason_FACE_NOT_FOUND
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype UnsuccessfulFaceDisassociationReason = UnsuccessfulFaceDisassociationReason'
  { fromUnsuccessfulFaceDisassociationReason ::
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

pattern UnsuccessfulFaceDisassociationReason_ASSOCIATED_TO_A_DIFFERENT_USER :: UnsuccessfulFaceDisassociationReason
pattern UnsuccessfulFaceDisassociationReason_ASSOCIATED_TO_A_DIFFERENT_USER = UnsuccessfulFaceDisassociationReason' "ASSOCIATED_TO_A_DIFFERENT_USER"

pattern UnsuccessfulFaceDisassociationReason_FACE_NOT_FOUND :: UnsuccessfulFaceDisassociationReason
pattern UnsuccessfulFaceDisassociationReason_FACE_NOT_FOUND = UnsuccessfulFaceDisassociationReason' "FACE_NOT_FOUND"

{-# COMPLETE
  UnsuccessfulFaceDisassociationReason_ASSOCIATED_TO_A_DIFFERENT_USER,
  UnsuccessfulFaceDisassociationReason_FACE_NOT_FOUND,
  UnsuccessfulFaceDisassociationReason'
  #-}
