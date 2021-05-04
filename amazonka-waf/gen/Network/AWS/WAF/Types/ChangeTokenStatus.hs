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
-- Module      : Network.AWS.WAF.Types.ChangeTokenStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.ChangeTokenStatus
  ( ChangeTokenStatus
      ( ..,
        ChangeTokenStatus_INSYNC,
        ChangeTokenStatus_PENDING,
        ChangeTokenStatus_PROVISIONED
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ChangeTokenStatus = ChangeTokenStatus'
  { fromChangeTokenStatus ::
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

pattern ChangeTokenStatus_INSYNC :: ChangeTokenStatus
pattern ChangeTokenStatus_INSYNC = ChangeTokenStatus' "INSYNC"

pattern ChangeTokenStatus_PENDING :: ChangeTokenStatus
pattern ChangeTokenStatus_PENDING = ChangeTokenStatus' "PENDING"

pattern ChangeTokenStatus_PROVISIONED :: ChangeTokenStatus
pattern ChangeTokenStatus_PROVISIONED = ChangeTokenStatus' "PROVISIONED"

{-# COMPLETE
  ChangeTokenStatus_INSYNC,
  ChangeTokenStatus_PENDING,
  ChangeTokenStatus_PROVISIONED,
  ChangeTokenStatus'
  #-}
