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
-- Module      : Amazonka.Shield.Types.CountAction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Shield.Types.CountAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies that Shield Advanced should configure its WAF rules with the
-- WAF @Count@ action.
--
-- This is only used in the context of the @ResponseAction@ setting.
--
-- JSON specification: @\"Count\": {}@
--
-- /See:/ 'newCountAction' smart constructor.
data CountAction = CountAction'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CountAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newCountAction ::
  CountAction
newCountAction = CountAction'

instance Data.FromJSON CountAction where
  parseJSON =
    Data.withObject
      "CountAction"
      (\x -> Prelude.pure CountAction')

instance Prelude.Hashable CountAction where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData CountAction where
  rnf _ = ()

instance Data.ToJSON CountAction where
  toJSON = Prelude.const (Data.Object Prelude.mempty)
