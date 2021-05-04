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
-- Module      : Network.AWS.EC2.Types.FpgaImageAttributeName
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FpgaImageAttributeName
  ( FpgaImageAttributeName
      ( ..,
        FpgaImageAttributeName_Description,
        FpgaImageAttributeName_LoadPermission,
        FpgaImageAttributeName_Name,
        FpgaImageAttributeName_ProductCodes
      ),
  )
where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Prelude as Prelude

newtype FpgaImageAttributeName = FpgaImageAttributeName'
  { fromFpgaImageAttributeName ::
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
