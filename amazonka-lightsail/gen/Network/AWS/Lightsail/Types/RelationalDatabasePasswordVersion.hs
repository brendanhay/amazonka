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
-- Module      : Network.AWS.Lightsail.Types.RelationalDatabasePasswordVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.RelationalDatabasePasswordVersion
  ( RelationalDatabasePasswordVersion
      ( ..,
        RelationalDatabasePasswordVersion_CURRENT,
        RelationalDatabasePasswordVersion_PENDING,
        RelationalDatabasePasswordVersion_PREVIOUS
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype RelationalDatabasePasswordVersion = RelationalDatabasePasswordVersion'
  { fromRelationalDatabasePasswordVersion ::
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

pattern RelationalDatabasePasswordVersion_CURRENT :: RelationalDatabasePasswordVersion
pattern RelationalDatabasePasswordVersion_CURRENT = RelationalDatabasePasswordVersion' "CURRENT"

pattern RelationalDatabasePasswordVersion_PENDING :: RelationalDatabasePasswordVersion
pattern RelationalDatabasePasswordVersion_PENDING = RelationalDatabasePasswordVersion' "PENDING"

pattern RelationalDatabasePasswordVersion_PREVIOUS :: RelationalDatabasePasswordVersion
pattern RelationalDatabasePasswordVersion_PREVIOUS = RelationalDatabasePasswordVersion' "PREVIOUS"

{-# COMPLETE
  RelationalDatabasePasswordVersion_CURRENT,
  RelationalDatabasePasswordVersion_PENDING,
  RelationalDatabasePasswordVersion_PREVIOUS,
  RelationalDatabasePasswordVersion'
  #-}
