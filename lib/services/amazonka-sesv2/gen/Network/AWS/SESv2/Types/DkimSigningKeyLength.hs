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
-- Module      : Network.AWS.SESv2.Types.DkimSigningKeyLength
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SESv2.Types.DkimSigningKeyLength
  ( DkimSigningKeyLength
      ( ..,
        DkimSigningKeyLength_RSA_1024_BIT,
        DkimSigningKeyLength_RSA_2048_BIT
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype DkimSigningKeyLength = DkimSigningKeyLength'
  { fromDkimSigningKeyLength ::
      Core.Text
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
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
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
