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
-- Module      : Amazonka.CloudFront.Types.OriginAccessControlSigningBehaviors
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.OriginAccessControlSigningBehaviors
  ( OriginAccessControlSigningBehaviors
      ( ..,
        OriginAccessControlSigningBehaviors_Always,
        OriginAccessControlSigningBehaviors_Never,
        OriginAccessControlSigningBehaviors_No_override
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype OriginAccessControlSigningBehaviors = OriginAccessControlSigningBehaviors'
  { fromOriginAccessControlSigningBehaviors ::
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

pattern OriginAccessControlSigningBehaviors_Always :: OriginAccessControlSigningBehaviors
pattern OriginAccessControlSigningBehaviors_Always = OriginAccessControlSigningBehaviors' "always"

pattern OriginAccessControlSigningBehaviors_Never :: OriginAccessControlSigningBehaviors
pattern OriginAccessControlSigningBehaviors_Never = OriginAccessControlSigningBehaviors' "never"

pattern OriginAccessControlSigningBehaviors_No_override :: OriginAccessControlSigningBehaviors
pattern OriginAccessControlSigningBehaviors_No_override = OriginAccessControlSigningBehaviors' "no-override"

{-# COMPLETE
  OriginAccessControlSigningBehaviors_Always,
  OriginAccessControlSigningBehaviors_Never,
  OriginAccessControlSigningBehaviors_No_override,
  OriginAccessControlSigningBehaviors'
  #-}
