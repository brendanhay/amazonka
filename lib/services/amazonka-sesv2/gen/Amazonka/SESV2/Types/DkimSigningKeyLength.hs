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
-- Module      : Amazonka.SESV2.Types.DkimSigningKeyLength
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.DkimSigningKeyLength
  ( DkimSigningKeyLength
      ( ..,
        DkimSigningKeyLength_RSA_1024_BIT,
        DkimSigningKeyLength_RSA_2048_BIT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DkimSigningKeyLength = DkimSigningKeyLength'
  { fromDkimSigningKeyLength ::
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

pattern DkimSigningKeyLength_RSA_1024_BIT :: DkimSigningKeyLength
pattern DkimSigningKeyLength_RSA_1024_BIT = DkimSigningKeyLength' "RSA_1024_BIT"

pattern DkimSigningKeyLength_RSA_2048_BIT :: DkimSigningKeyLength
pattern DkimSigningKeyLength_RSA_2048_BIT = DkimSigningKeyLength' "RSA_2048_BIT"

{-# COMPLETE
  DkimSigningKeyLength_RSA_1024_BIT,
  DkimSigningKeyLength_RSA_2048_BIT,
  DkimSigningKeyLength'
  #-}
