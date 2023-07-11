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
-- Module      : Amazonka.EC2.Types.FpgaImageAttributeName
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.FpgaImageAttributeName
  ( FpgaImageAttributeName
      ( ..,
        FpgaImageAttributeName_Description,
        FpgaImageAttributeName_LoadPermission,
        FpgaImageAttributeName_Name,
        FpgaImageAttributeName_ProductCodes
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype FpgaImageAttributeName = FpgaImageAttributeName'
  { fromFpgaImageAttributeName ::
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

pattern FpgaImageAttributeName_Description :: FpgaImageAttributeName
pattern FpgaImageAttributeName_Description = FpgaImageAttributeName' "description"

pattern FpgaImageAttributeName_LoadPermission :: FpgaImageAttributeName
pattern FpgaImageAttributeName_LoadPermission = FpgaImageAttributeName' "loadPermission"

pattern FpgaImageAttributeName_Name :: FpgaImageAttributeName
pattern FpgaImageAttributeName_Name = FpgaImageAttributeName' "name"

pattern FpgaImageAttributeName_ProductCodes :: FpgaImageAttributeName
pattern FpgaImageAttributeName_ProductCodes = FpgaImageAttributeName' "productCodes"

{-# COMPLETE
  FpgaImageAttributeName_Description,
  FpgaImageAttributeName_LoadPermission,
  FpgaImageAttributeName_Name,
  FpgaImageAttributeName_ProductCodes,
  FpgaImageAttributeName'
  #-}
