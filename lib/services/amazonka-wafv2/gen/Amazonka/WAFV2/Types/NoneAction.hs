{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.WAFV2.Types.NoneAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.NoneAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies that WAF should do nothing. This is used for the
-- @OverrideAction@ setting on a Rule when the rule uses a rule group
-- reference statement.
--
-- This is used in the context of other settings, for example to specify
-- values for RuleAction and web ACL DefaultAction.
--
-- JSON specification: @\"None\": {}@
--
-- /See:/ 'newNoneAction' smart constructor.
data NoneAction = NoneAction'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NoneAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newNoneAction ::
  NoneAction
newNoneAction = NoneAction'

instance Data.FromJSON NoneAction where
  parseJSON =
    Data.withObject
      "NoneAction"
      (\x -> Prelude.pure NoneAction')

instance Prelude.Hashable NoneAction where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData NoneAction where
  rnf _ = ()

instance Data.ToJSON NoneAction where
  toJSON = Prelude.const (Data.Object Prelude.mempty)
